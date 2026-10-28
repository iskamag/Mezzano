#!/usr/bin/env python3
"""Host-side protocol tests.  No VM or external Python packages are needed."""
import json
import os
from pathlib import Path
import re
import socket
import subprocess
import sys
import tempfile
import threading
import time
import unittest

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
import agent


class FakeSerial:
    def __init__(self, path, handler):
        self.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socket.bind(str(path))
        self.socket.listen(2)
        self.socket.settimeout(3)
        self.error = None
        self.received = bytearray()
        self.handler = handler
        self.thread = threading.Thread(target=self.serve, daemon=True)
        self.thread.start()

    def serve(self):
        try:
            conn, _ = self.socket.accept()
            with conn:
                conn.settimeout(3)
                while True:
                    data = conn.recv(1)
                    if not data:
                        break
                    self.received.extend(data)
                    conn.sendall(data)
                    if data == b"\n":
                        self.handler(conn, bytes(self.received))
                        break
        except Exception as exc:
            self.error = exc
        finally:
            self.socket.close()

    def join(self):
        self.thread.join(5)
        if self.thread.is_alive():
            raise AssertionError("fake serial server did not stop")
        if self.error:
            raise self.error


def reply(conn, request):
    match = re.search(rb"AGENT[0-9A-F]{32}", request)
    assert match, request
    # Deliberately send fragmented replies: the parser must not depend on
    # packet boundaries and must not mistake the echoed token for this reply.
    for byte in b"\r\n" + match.group() + b" STARTED\r\nNIL\r\nCL-USER> ":
        conn.sendall(bytes([byte]))


class AgentTests(unittest.TestCase):
    def test_serial_waits_for_reply_after_echo(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "serial.sock"
            fake = FakeSerial(path, reply)
            token = "AGENT" + "A" * 32
            form = f'(progn "{token}" (values))'
            result, transcript, prompt = agent.serial_command(path, form, token, 4)
            fake.join()
            self.assertEqual(result, "STARTED")
            self.assertTrue(prompt)
            self.assertIn(token.encode() + b" STARTED", transcript)
            self.assertEqual(fake.received, (form + "\n").encode())

    def test_partial_transmission_is_unknown(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "serial.sock"
            server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            server.bind(str(path))
            server.listen(1)
            def no_echo():
                conn, _ = server.accept()
                with conn:
                    conn.recv(1)
                    time.sleep(0.4)
                server.close()
            thread = threading.Thread(target=no_echo, daemon=True)
            thread.start()
            with self.assertRaises(agent.SerialFailure) as caught:
                agent.serial_command(path, "(nil)", "AGENT" + "A" * 32, 0.2)
            self.assertTrue(caught.exception.sent)
            thread.join(2)

    def test_acknowledgement_survives_missing_later_prompt(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "serial.sock"
            token = "AGENT" + "A" * 32
            fake = FakeSerial(path, lambda conn, request: conn.sendall(
                b"\r\n" + token.encode() + b" STARTED\r\n"))
            result, _, prompt = agent.serial_command(path, f'(print "{token}")', token, 4)
            fake.join()
            self.assertEqual(result, "STARTED")
            self.assertFalse(prompt)

    def test_prompt_must_be_current_and_not_debugger(self):
        with tempfile.TemporaryDirectory() as temp:
            path = Path(temp) / "serial.sock"
            server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            server.bind(str(path))
            log = path.with_name("serial.log")
            log.write_text("boot\nOTHER.PACKAGE> ")
            agent.check_serial_prompt(path)
            for tail in ("CL-USER/0] ", "CL-USER> (incomplete", "panic\n"):
                log.write_text(tail)
                with self.assertRaises(ValueError):
                    agent.check_serial_prompt(path)
            log.write_text("CL-USER> ")
            old = path.stat().st_ctime_ns - 1_000_000_000
            os.utime(log, ns=(old, old))
            with self.assertRaises(ValueError):
                agent.check_serial_prompt(path)
            server.close()

    def test_job_states_and_replaced_socket(self):
        with tempfile.TemporaryDirectory() as temp:
            home = Path(temp)
            serial = home / "serial.sock"
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.bind(str(serial))
            st = serial.stat()
            job_id = "b" * 32
            directory = agent.root_for(home) / job_id
            directory.mkdir()
            agent.write_json(directory / "job.json", {"package": "CL-USER", "serial": str(serial)})
            agent.write_json(directory / "dispatch.json", {"state": "sent", "serial_identity": [st.st_dev, st.st_ino]})
            self.assertEqual(agent.snapshot(home, job_id)["state"], "starting")
            (directory / "started").write_text("1")
            self.assertEqual(agent.snapshot(home, job_id)["state"], "running")
            (directory / "input-ready").write_text("1")
            self.assertEqual(agent.snapshot(home, job_id)["state"], "input")
            (directory / "debug-ready").write_text("1")
            self.assertEqual(agent.snapshot(home, job_id)["state"], "debug")
            serial.unlink()
            self.assertEqual(agent.snapshot(home, job_id)["state"], "lost")
            (directory / "values.txt").write_text("42\n")
            (directory / "ok").write_text("1")
            result = agent.snapshot(home, job_id)
            self.assertEqual((result["state"], result["values"]), ("ok", "42\n"))
            sock.close()

    def test_async_cli_keeps_source_out_of_serial(self):
        with tempfile.TemporaryDirectory() as temp:
            home = Path(temp) / "home"
            home.mkdir()
            serial = Path(temp) / "serial.sock"
            source = '(progn #| ) |# (format t "Zażółć") #\\))\r\n'
            source_file = Path(temp) / "region.lisp"
            source_file.write_bytes(source.encode("utf-8"))
            def handle(conn, request):
                job_id = re.search(rb'"([0-9a-f]{32})"', request).group(1).decode()
                directory = home / ".mezzano-agent" / job_id
                self.assertEqual((directory / "source.lisp").read_bytes().decode("utf-8"), source)
                self.assertNotIn(source.encode(), request)
                (directory / "output.txt").write_text("Zażółć")
                (directory / "started").write_text("1")
                (directory / "values.txt").write_text("42\n")
                (directory / "ok").write_text("1")
                reply(conn, request)
            fake = FakeSerial(serial, handle)
            serial.with_name("serial.log").write_text("CL-USER> ")
            base = [sys.executable, str(Path(agent.__file__)), "--home", str(home), "--serial", str(serial)]
            start = time.monotonic()
            initial = json.loads(subprocess.check_output(base + ["eval", "--file", str(source_file)], text=True))
            self.assertLess(time.monotonic() - start, 2)
            self.assertIn(initial["state"], {"queued", "dispatching", "ok"})
            job = initial["id"]
            result = json.loads(subprocess.check_output(base + ["wait", job, "--timeout", "4"], text=True))
            self.assertEqual(result["state"], "ok")
            self.assertEqual(result["values"], "42\n")
            self.assertEqual(subprocess.check_output(base + ["output", job], text=True), "Zażółć")
            fake.join()


if __name__ == "__main__":
    unittest.main(verbosity=2)
