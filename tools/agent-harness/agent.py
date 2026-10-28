#!/usr/bin/env python3
"""Submit Mezzano Lisp jobs over the serial listener; poll their files locally."""

import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import select
import socket
import subprocess
import sys
import tempfile
import time
import uuid

ROOT = Path(__file__).resolve().parents[3]
DEFAULT_HOME = ROOT / "home"
DEFAULT_SERIAL = ROOT / "serial.sock"
JOB_ID = re.compile(r"^[0-9a-f]{32}$")
TERMINAL = {"ok", "aborted", "cancelled", "failed", "lost", "dispatch_error", "dispatch_unknown"}


def atomic_write(path, content):
    path = Path(path)
    tmp = path.with_name("." + path.name + "." + uuid.uuid4().hex)
    try:
        with open(tmp, "w", encoding="utf-8", newline="") as stream:
            stream.write(content)
        os.replace(tmp, path)
    finally:
        try:
            tmp.unlink()
        except FileNotFoundError:
            pass


def write_json(path, value):
    atomic_write(path, json.dumps(value, ensure_ascii=False) + "\n")


def read_json(path, default=None):
    try:
        with open(path, encoding="utf-8") as stream:
            return json.load(stream)
    except FileNotFoundError:
        return default


def root_for(home):
    root = Path(home).resolve() / ".mezzano-agent"
    root.mkdir(mode=0o700, exist_ok=True)
    if not root.is_dir():
        raise ValueError(f"not a directory: {root}")
    return root


def job_dir(home, job_id):
    if not JOB_ID.fullmatch(job_id):
        raise ValueError("job ID must be 32 lowercase hexadecimal characters")
    path = root_for(home) / job_id
    if not path.is_dir():
        raise ValueError(f"unknown job: {job_id}")
    return path


def process_alive(pid):
    if not pid:
        return False
    try:
        os.kill(pid, 0)
        stat = Path(f"/proc/{pid}/stat")
        if stat.exists() and stat.read_text().rsplit(")", 1)[1].split()[0] == "Z":
            return False
        return True
    except (ProcessLookupError, FileNotFoundError):
        return False
    except PermissionError:
        return True


def marker_number(path):
    try:
        value = path.read_text(encoding="ascii").strip()
        return int(value) if value.isdecimal() else None
    except FileNotFoundError:
        return None


def snapshot(home, job_id):
    directory = job_dir(home, job_id)
    meta = read_json(directory / "job.json", {})
    dispatch = read_json(directory / "dispatch.json", {"state": "queued"})
    result = {"id": job_id, "package": meta.get("package"),
              "created": meta.get("created"), "state": "queued", "dispatch": dispatch}
    for name in ("ok", "aborted", "cancelled", "failed"):
        if (directory / name).exists():
            result["state"] = name
            break
    else:
        debug_generation = marker_number(directory / "debug-ready")
        input_generation = marker_number(directory / "input-ready")
        if debug_generation is not None:
            result["state"] = "debug"
            result["generation"] = debug_generation
        elif input_generation is not None:
            result["state"] = "input"
            result["generation"] = input_generation
        elif (directory / "started").exists():
            result["state"] = "running"
        elif dispatch.get("state") in ("error", "unknown"):
            result["state"] = "dispatch_" + dispatch["state"]
        elif dispatch.get("state") == "sent":
            result["state"] = "starting"
        else:
            result["state"] = dispatch.get("state", "queued")
            pid_file = read_json(directory / "dispatcher-pid.json", {})
            pid = pid_file.get("pid")
            if pid and not process_alive(pid):
                latest = read_json(directory / "dispatch.json", {})
                if latest != dispatch or (directory / "started").exists() or any(
                    (directory / name).exists() for name in ("ok", "aborted", "cancelled", "failed")
                ):
                    return snapshot(home, job_id)
                result["state"] = "dispatch_unknown"
                result["dispatch"] = {"state": "unknown", "error": "dispatcher exited before confirming the command"}
    identity = dispatch.get("serial_identity")
    if result["state"] in {"running", "debug", "input", "starting"} and identity:
        try:
            stat = Path(meta["serial"]).stat()
            alive = [stat.st_dev, stat.st_ino] == identity
        except (OSError, KeyError):
            alive = False
        if not alive:
            result["state"] = "lost"
            result["reason"] = "the QEMU serial socket disappeared or was replaced"
    for filename, field in (("values.txt", "values"), ("debug.txt", "debug"),
                            ("failure.txt", "failure")):
        if field == "debug" and result["state"] != "debug":
            continue
        if field == "values" and result["state"] != "ok":
            continue
        try:
            result[field] = (directory / filename).read_text(encoding="utf-8", errors="replace")
        except FileNotFoundError:
            pass
    try:
        result["output_bytes"] = (directory / "output.txt").stat().st_size
    except FileNotFoundError:
        result["output_bytes"] = 0
    return result


def lisp_string(value):
    # Common Lisp escaping differs from JSON: newlines must be literal.
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def command_form(action, job_id, index, token, input_ref=None, generation=None):
    fn = {"submit": "SUBMIT", "abort": "ABORT-JOB", "restart": "RESTART-JOB",
          "input": "INPUT-JOB"}[action]
    args = lisp_string(job_id) + (f" {index} {generation}" if action == "restart" else "")
    if action == "input":
        args += " " + lisp_string(input_ref) + " " + str(generation)
    # Resolve the function after LOAD so the form can be read before the
    # package exists.  The marker is only emitted after the operation returns.
    return ("(handler-case (let* ((p (find-package \"MEZZANO.AGENT-HARNESS\")) "
            "(v (and p (find-symbol \"*PROTOCOL-VERSION*\" p)))) "
            "(unless (and (equal (mezzano.supervisor:thread-name "
            "(mezzano.supervisor:current-thread)) \"Debug Serial Lisp Listener\") "
            "(zerop mezzano.internals::*debugger-depth*)) "
            "(error \"Serial reader is not the idle listener\")) "
            "(unless (and v (boundp v) (eql (symbol-value v) 3)) "
            "(load (merge-pathnames \"tools/agent-harness/guest.lisp\" "
            "mezzano.internals::*mezzano-source-path*)) "
            "(setf p (find-package \"MEZZANO.AGENT-HARNESS\"))) "
            f"(format t \"~%{token} ~A~%\" (funcall (find-symbol \"{fn}\" p) {args}))) "
            f"(error (c) (format t \"~%{token} ERROR~%~A~%\" c)))")


class SerialFailure(Exception):
    def __init__(self, message, sent):
        super().__init__(message)
        self.sent = sent


def check_serial_prompt(path):
    """Require a prompt from the current QEMU instance before writing anything."""
    path = Path(path)
    log = path.with_name("serial.log")
    try:
        sock_stat, log_stat = path.stat(), log.stat()
        if log_stat.st_mtime_ns < sock_stat.st_ctime_ns:
            raise ValueError("serial log predates the current socket; guest has not reached a fresh prompt")
        with open(log, "rb") as stream:
            stream.seek(0, os.SEEK_END)
            stream.seek(max(0, stream.tell() - 200000))
            tail = stream.read().decode("utf-8", errors="replace").replace("\r", "")
    except FileNotFoundError as exc:
        raise ValueError(f"serial socket or log is missing: {exc.filename}") from exc
    # The listener prints its current package name, which may change.  Require
    # the prompt at the end: debugger output, a partial form, a still-busy REPL,
    # or asynchronous output must be inspected before another command is sent.
    if not re.search(r"(?:^|\n)[^\n\[\]<>]+> *$", tail):
        raise ValueError("serial log does not end at a listener prompt; inspect it before dispatching")


def serial_command(path, form, token, timeout):
    """Send one ASCII form with at most one unacknowledged UART byte.

    The guest serial stream echoes characters as it consumes them.  A unique
    marker printed by the evaluated form is the actual dispatch confirmation;
    echo alone does not prove that the requested operation ran.
    """
    deadline = time.monotonic() + timeout
    transmitted = 0
    seen = bytearray()
    serial = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        serial.settimeout(min(timeout, 3))
        serial.connect(str(path))
        serial.setblocking(False)

        def get_byte():
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not select.select([serial], [], [], max(0, remaining))[0]:
                raise TimeoutError("timed out waiting for serial output")
            data = serial.recv(1)
            if not data:
                raise ConnectionError("serial socket closed")
            return data

        for byte in form.encode("ascii"):
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not select.select([], [serial], [], max(0, remaining))[1]:
                raise TimeoutError("timed out writing to serial socket")
            serial.sendall(bytes([byte]))
            transmitted += 1
            while True:
                incoming = get_byte()
                if incoming == bytes([byte]):
                    break
                if len(seen) < 65536:
                    seen.extend(incoming)
        # Mezzano READ may wait for a delimiter after the final parenthesis.
        # All form bytes have already been echoed, so the final newline is the
        # only unacknowledged UART byte and cannot expose the token in the echo.
        serial.sendall(b"\n")
        transmitted += 1
        response = bytearray()
        marker = re.compile(re.escape(token.encode("ascii")) + rb" ([A-Z-]+)\r?\n")
        reply = None
        prompt = re.compile(rb"(?:^|\n)[^\r\n\[\]<>]+> ")
        while True:
            try:
                response.extend(get_byte())
            except (TimeoutError, OSError):
                if reply is not None:
                    return reply, bytes(seen + response), False
                raise
            match = marker.search(response)
            if match:
                if reply is None:
                    reply = match.group(1).decode("ascii")
                    # The operation is already confirmed. Do not hold up
                    # unrelated dispatchers for the full command timeout if
                    # the listener fails to return to its prompt afterwards.
                    deadline = min(deadline, time.monotonic() + 2)
                if prompt.search(response, match.end()):
                    return reply, bytes(seen + response), True
            if len(response) > 2_000_000:
                if reply is not None:
                    return reply, bytes(seen + response), False
                raise ValueError("serial response exceeded 2 MB without an acknowledgement")
    except Exception as exc:
        raise SerialFailure(str(exc), transmitted > 0) from exc
    finally:
        serial.close()


def dispatch_worker(home, serial, job_id, action, index, control_id, timeout, input_ref=None,
                    generation=None):
    directory = job_dir(home, job_id)
    status_path = directory / ("dispatch.json" if action == "submit" else f"control-{control_id}.json")
    token = "AGENT" + uuid.uuid4().hex.upper()
    form = command_form(action, job_id, index, token, input_ref, generation)
    lock_root = Path(tempfile.gettempdir()) / f"mezzano-agent-{os.getuid()}"
    identity = None
    sent_possible = False
    try:
        lock_root.mkdir(mode=0o700, exist_ok=True)
        st = lock_root.stat()
        if lock_root.is_symlink() or st.st_uid != os.getuid() or st.st_mode & 0o077:
            raise ValueError(f"insecure serial lock directory: {lock_root}")
        lock_name = hashlib.sha256(str(Path(serial).resolve()).encode()).hexdigest()
        lock_path = lock_root / lock_name
        with open(lock_path, "a+") as lock:
            # Other dispatchers wait in the background, with a bounded deadline.
            lock_deadline = time.monotonic() + timeout
            while True:
                try:
                    fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                    break
                except BlockingIOError:
                    if time.monotonic() >= lock_deadline:
                        raise TimeoutError("timed out waiting for the serial dispatcher lock")
                    time.sleep(0.1)
            write_json(status_path, {"state": "dispatching", "action": action})
            check_serial_prompt(serial)
            stat = Path(serial).stat()
            identity = [stat.st_dev, stat.st_ino]
            write_json(status_path, {"state": "dispatching", "action": action,
                                     "serial_identity": identity})
            reply, transcript, prompt_ready = serial_command(serial, form, token, timeout)
            sent_possible = True
            accepted = {"submit": {"STARTED", "DUPLICATE"},
                        "abort": {"REQUESTED"}, "restart": {"REQUESTED"},
                        "input": {"REQUESTED"}}[action]
            state = "sent" if reply in accepted else "error"
            write_json(status_path, {"state": state, "action": action, "reply": reply,
                                     "serial_prompt": prompt_ready, "serial_identity": identity})
            if state == "error":
                atomic_write(directory / f"serial-{control_id or 'submit'}.log",
                             transcript.decode("utf-8", errors="replace"))
    except SerialFailure as exc:
        # A partially transmitted command may still execute later.  Never retry
        # automatically or claim that it definitely did not run.
        write_json(status_path, {"state": "unknown" if exc.sent else "error",
                                 "action": action, "error": str(exc), "serial_identity": identity})
    except Exception as exc:
        write_json(status_path, {"state": "unknown" if sent_possible else "error",
                                 "action": action, "error": str(exc), "serial_identity": identity})


def launch_dispatch(home, serial, job_id, action, index=None, timeout=30,
                    control_id=None, input_ref=None, generation=None):
    directory = job_dir(home, job_id)
    control_id = (control_id or uuid.uuid4().hex) if action != "submit" else ""
    path = directory / (f"control-{control_id}.json" if control_id else "dispatch.json")
    write_json(path, {"state": "queued", "action": action})
    cmd = [sys.executable, str(Path(__file__).resolve()), "_dispatch", job_id,
           "--home", str(Path(home).resolve()), "--serial", str(Path(serial).resolve()),
           "--action", action, "--control-id", control_id, "--timeout", str(timeout)]
    if index is not None:
        cmd.extend(["--index", str(index)])
    if input_ref is not None:
        cmd.extend(["--input-ref", input_ref])
    if generation is not None:
        cmd.extend(["--generation", str(generation)])
    try:
        with open(directory / "dispatcher.log", "ab") as log:
            process = subprocess.Popen(cmd, stdin=subprocess.DEVNULL, stdout=log, stderr=log,
                                       start_new_session=True, close_fds=True)
    except OSError as exc:
        write_json(path, {"state": "error", "action": action, "error": str(exc)})
        return control_id
    # The child may already be sending the command. A failure to record its
    # PID must not be reported as a definite failure to submit.
    pid_name = "dispatcher-pid.json" if action == "submit" else f"control-{control_id}-pid.json"
    try:
        write_json(directory / pid_name, {"pid": process.pid})
    except OSError:
        pass
    return control_id


def wait_for(home, job_id, seconds):
    deadline = time.monotonic() + max(0, seconds)
    while True:
        result = snapshot(home, job_id)
        if result["state"] in TERMINAL or result["state"] in {"debug", "input"}:
            return result
        if time.monotonic() >= deadline:
            return result
        time.sleep(min(0.1, max(0, deadline - time.monotonic())))


def print_json(value):
    print(json.dumps(value, ensure_ascii=False, indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--home", type=Path, default=DEFAULT_HOME, help="host directory shared as the guest's home")
    parser.add_argument("--serial", type=Path, default=DEFAULT_SERIAL, help="QEMU serial Unix socket")
    commands = parser.add_subparsers(dest="command", required=True)
    run = commands.add_parser("eval", help="submit source and return a job ID")
    source = run.add_mutually_exclusive_group(required=True)
    source.add_argument("--expr", help="Lisp source text")
    source.add_argument("--file", type=Path, help="read Lisp source from this host file")
    source.add_argument("--stdin", action="store_true", help="read Lisp source from standard input")
    run.add_argument("--package", default="CL-USER")
    run.add_argument("--wait", type=float, default=0, help="wait at most this many seconds for a result")
    show = commands.add_parser("show", help="show job state, values, and debugger information")
    show.add_argument("id")
    jobs = commands.add_parser("jobs", help="list recent jobs")
    jobs.add_argument("--limit", type=int, default=20)
    wait = commands.add_parser("wait", help="wait for completion or debugger, with a deadline")
    wait.add_argument("id")
    wait.add_argument("--timeout", type=float, default=5)
    output = commands.add_parser("output", help="print captured output")
    output.add_argument("id")
    output.add_argument("--offset", type=int, default=0, help="byte offset into output")
    output.add_argument("--max-bytes", type=int, default=0, help="maximum bytes; zero means all")
    abort = commands.add_parser("abort", help="request cancellation without blocking")
    abort.add_argument("id")
    restart = commands.add_parser("restart", help="invoke a numbered debugger restart without arguments")
    restart.add_argument("id")
    restart.add_argument("index", type=int)
    restart.add_argument("--generation", type=int, help="debugger generation from show")
    input_command = commands.add_parser("input", help="supply input to a waiting job")
    input_command.add_argument("id")
    input_source = input_command.add_mutually_exclusive_group(required=True)
    input_source.add_argument("--text")
    input_source.add_argument("--file", type=Path)
    input_source.add_argument("--eof", action="store_true")
    input_command.add_argument("--raw", action="store_true", help="do not append a newline to --text")
    input_command.add_argument("--generation", type=int, help="input generation from show")
    control = commands.add_parser("control", help="inspect the result of an abort/restart request")
    control.add_argument("id")
    control.add_argument("control_id")
    worker = commands.add_parser("_dispatch", help="internal dispatcher")
    worker.add_argument("id")
    worker.add_argument("--home", type=Path, required=True)
    worker.add_argument("--serial", type=Path, required=True)
    worker.add_argument("--action", choices=("submit", "abort", "restart", "input"), required=True)
    worker.add_argument("--control-id", required=True)
    worker.add_argument("--index", type=int)
    worker.add_argument("--input-ref")
    worker.add_argument("--generation", type=int)
    worker.add_argument("--timeout", type=float, required=True)
    args = parser.parse_args()
    try:
        if args.command == "_dispatch":
            dispatch_worker(args.home, args.serial, args.id, args.action, args.index, args.control_id,
                            args.timeout, args.input_ref, args.generation)
        elif args.command == "eval":
            if not args.home.is_dir():
                raise ValueError(f"shared home directory does not exist: {args.home}")
            text = (args.expr if args.expr is not None else
                    (args.file.read_bytes() if args.file else sys.stdin.buffer.read()).decode("utf-8"))
            if "\n" in args.package or "\r" in args.package or not args.package:
                raise ValueError("package must be a non-empty single line")
            job_id = uuid.uuid4().hex
            directory = root_for(args.home) / job_id
            directory.mkdir(mode=0o700)
            atomic_write(directory / "source.lisp", text)
            atomic_write(directory / "package.txt", args.package + "\n")
            write_json(directory / "job.json", {"id": job_id, "package": args.package, "created": time.time(),
                                                 "serial": str(args.serial.resolve())})
            launch_dispatch(args.home, args.serial, job_id, "submit")
            result = wait_for(args.home, job_id, args.wait)
            print_json(result)
            if result["state"] in (TERMINAL - {"ok"}) or result["state"] == "debug":
                return 1
        elif args.command == "show":
            print_json(snapshot(args.home, args.id))
        elif args.command == "jobs":
            if args.limit < 1:
                raise ValueError("limit must be positive")
            entries = []
            for directory in root_for(args.home).iterdir():
                if directory.is_dir() and JOB_ID.fullmatch(directory.name):
                    metadata = read_json(directory / "job.json", {})
                    entries.append((metadata.get("created", 0), directory.name))
            result = []
            for _, job_id in sorted(entries, reverse=True)[:args.limit]:
                state = snapshot(args.home, job_id)
                result.append({key: state[key] for key in ("id", "state", "package", "created", "output_bytes")})
            print_json(result)
        elif args.command == "wait":
            result = wait_for(args.home, args.id, args.timeout)
            print_json(result)
            if result["state"] == "ok":
                return 0
            return 1 if result["state"] in TERMINAL or result["state"] in {"debug", "input"} else 124
        elif args.command == "output":
            if args.offset < 0 or args.max_bytes < 0:
                raise ValueError("offset and max-bytes must be non-negative")
            path = job_dir(args.home, args.id) / "output.txt"
            if path.exists():
                with open(path, "rb") as stream:
                    stream.seek(args.offset)
                    remaining = args.max_bytes or None
                    while True:
                        data = stream.read(min(65536, remaining) if remaining is not None else 65536)
                        if not data:
                            break
                        sys.stdout.buffer.write(data)
                        if remaining is not None:
                            remaining -= len(data)
                            if not remaining:
                                break
        elif args.command in ("abort", "restart"):
            job_dir(args.home, args.id)
            generation = None
            if args.command == "restart":
                current = snapshot(args.home, args.id)
                if current["state"] != "debug":
                    raise ValueError("job is not in the debugger")
                generation = args.generation if args.generation is not None else current["generation"]
                if generation <= 0:
                    raise ValueError("generation must be positive")
            control_id = launch_dispatch(args.home, args.serial, args.id, args.command,
                                         getattr(args, "index", None), generation=generation)
            print_json({"id": args.id, "control_id": control_id, "state": "queued"})
        elif args.command == "input":
            directory = job_dir(args.home, args.id)
            current = snapshot(args.home, args.id)
            if current["state"] != "input":
                raise ValueError("job is not waiting for input")
            generation = args.generation if args.generation is not None else current["generation"]
            if generation <= 0:
                raise ValueError("generation must be positive")
            control_id = uuid.uuid4().hex
            if args.eof:
                input_ref = "EOF"
            else:
                data = args.text if args.text is not None else args.file.read_bytes().decode("utf-8")
                if args.text is not None and not args.raw:
                    data += "\n"
                atomic_write(directory / f"input-{control_id}.txt", data)
                input_ref = control_id
            launch_dispatch(args.home, args.serial, args.id, "input", control_id=control_id,
                            input_ref=input_ref, generation=generation)
            print_json({"id": args.id, "control_id": control_id, "state": "queued"})
        elif args.command == "control":
            if not JOB_ID.fullmatch(args.control_id):
                raise ValueError("control ID must be 32 lowercase hexadecimal characters")
            result = read_json(job_dir(args.home, args.id) / f"control-{args.control_id}.json")
            if result is None:
                raise ValueError("unknown control request")
            pid_info = read_json(job_dir(args.home, args.id) / f"control-{args.control_id}-pid.json", {})
            if result.get("state") in {"queued", "dispatching"} and pid_info.get("pid"):
                if not process_alive(pid_info["pid"]):
                    result = read_json(job_dir(args.home, args.id) / f"control-{args.control_id}.json", {})
                    if result.get("state") in {"queued", "dispatching"}:
                        result = {"state": "unknown", "error": "dispatcher exited before confirming the command"}
            print_json(result)
    except (ValueError, OSError) as exc:
        print(f"agent harness: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
