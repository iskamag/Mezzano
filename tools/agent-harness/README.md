# Agent harness for a running image

`tools/agent-harness/agent.py` evaluates Common Lisp in a running Mezzano image
without Swank or an Emacs session. It uses the existing serial listener to start
short control commands, and the remote file server to exchange source, output,
values, and debugger information. The first command loads
`tools/agent-harness/guest.lisp` into the running image; no rebuild is required.

From the MBuild directory, start the file server and guest in separate terminals:

```sh
make run-file-server
make serial-kvm
```

Wait until the serial listener has printed a prompt in `serial.log`. The guest's
`user-homedir-pathname` must refer to the host's `home/` directory and its
`mezzano.internals::*mezzano-source-path*` must refer to this source tree, as
in the normal MBuild configuration. The socket and log must be in the same
directory, named `serial.sock` and `serial.log`. If the host home or socket is
elsewhere, put `--home /host/shared/home --serial /path/serial.sock` **before**
the subcommand on every invocation.

```sh
# Submit immediately; the JSON response contains an ID and current state.
python3 Mezzano/tools/agent-harness/agent.py eval --expr '(+ 20 22)'

# Optionally wait briefly for a quick evaluation:
python3 Mezzano/tools/agent-harness/agent.py eval --expr '(values 42 :ok)' --wait 2

# Source is read by the guest Lisp reader, including multiple top-level forms.
python3 Mezzano/tools/agent-harness/agent.py eval --file /tmp/example.lisp --package CL-USER
cat /tmp/example.lisp | python3 Mezzano/tools/agent-harness/agent.py eval --stdin

python3 Mezzano/tools/agent-harness/agent.py show JOB_ID
python3 Mezzano/tools/agent-harness/agent.py jobs --limit 20
python3 Mezzano/tools/agent-harness/agent.py wait JOB_ID --timeout 5
python3 Mezzano/tools/agent-harness/agent.py output JOB_ID
python3 Mezzano/tools/agent-harness/agent.py output JOB_ID --offset 1024 --max-bytes 4096
```

Every evaluation gets its own guest thread. Other requests can run while an
existing request is sleeping, waiting for input, or in the debugger. The guest
uses its real reader and evaluates the forms in sequence; it does not split or
rewrite Lisp source on the host. The last form's values are in `values`; output
from standard, error, and trace streams is in `output.txt`. Output is flushed
after each form and when the program calls `finish-output` or `force-output`.
Use the `output_bytes` value as the next byte offset when fetching incrementally.
Values are printed with `*print-length*` 100, `*print-level*` 12, and
`*print-circle*` true; print explicitly to output if a different representation
is needed.

The default evaluation package is `CL-USER`. `--package` selects the initial
package by its actual name. An `in-package` form affects subsequent forms in the
same submission. Each submission has fresh dynamic bindings, so specify the
package again for another request; global definitions and other changes in the
image remain available. `--file` sends the file's contents as a region, not as a
`load`: it does not bind `*load-pathname*`. To load or compile a guest file,
submit an ordinary `(load ...)` or `(compile-file ...)` form instead.

`eval` normally returns as soon as the job is queued locally. `show` is an
immediate snapshot. `wait` defaults to five seconds and stops sooner on a
finished job, debugger, or input request. It exits 0 for `ok`, 124 if still
pending at the deadline, and 1 for other stopped states. `eval --wait` exits 1
if it observes a debugger or a failed job; a still-running job is a successful
submission. These deadlines never terminate the guest job.

## Input and debugging

When a job tries to read standard input, `show` reports `input` and a
`generation`. Prompt output is flushed before this state is published. Send
text or EOF and then poll again:

```sh
python3 Mezzano/tools/agent-harness/agent.py input JOB_ID --text 'Ada' --generation 1
python3 Mezzano/tools/agent-harness/agent.py input JOB_ID --file /tmp/input.txt --generation 2
python3 Mezzano/tools/agent-harness/agent.py input JOB_ID --eof --generation 3
```

`--text` adds a newline unless `--raw` is given. A file's bytes are used as-is
and must be UTF-8. Normal blocking character and line reads are supported;
`read-char-no-hang` returns immediately and does not create an input request.
Input and output streams belong to the job rather than the serial REPL.

An unhandled condition or `break` puts only that job in `debug`. `show` includes
the condition, numbered restarts, and a backtrace captured before unwinding.
The other jobs and serial listener continue running. Pick a restart that needs
no arguments, or abort the request:

```sh
python3 Mezzano/tools/agent-harness/agent.py restart JOB_ID 0 --generation 1
python3 Mezzano/tools/agent-harness/agent.py abort JOB_ID
```

`restart`, `input`, and `abort` return immediately with a control ID. Check its
actual disposition with `agent.py control JOB_ID CONTROL_ID`. `REQUESTED` means
the guest accepted the action, not that the job has completed; poll the job for
the outcome. Restarts requiring arguments are not supported. `abort` resumes an
input/debugger wait by unwinding it; for a running thread it requests Mezzano's
thread termination mechanism, which may be delayed where footholds are
inhibited. A control's `generation` defaults to the currently visible one;
pass the generation from `show` to ensure a delayed response cannot be applied
to a later input request or debugger stop.

## State and recovery

Jobs and their output live in `home/.mezzano-agent/JOB_ID/`. Submission first
writes the source and package files atomically; only a short command containing
the ID goes over serial. Dispatchers serialize access to the socket, pace serial
bytes using the guest's echo, and require a unique acknowledgement after the
command executes. They check that the command is executing on the serial
listener outside the debugger, and require the current log to end at a serial REPL prompt;
a debugger, a partial form, another client, or unrelated later serial output can
prevent safe dispatch. Only one serial client may be attached at a time.
Inspect `serial.log` and resolve the console state before submitting again.

`dispatch_error` means dispatch failed before transmitting or the guest rejected
the command. `dispatch_unknown` means the command might have been partially
sent or might already have run. The harness does not automatically retry it:
check the job files and serial log, and recover any partially entered form
before issuing another request. A confirmed job whose QEMU serial socket is
removed or replaced is shown as `lost`. This detects replacement of the QEMU
socket, not every possible guest hang or an in-place reset. Similarly, a broken
file-server connection may prevent a live job from publishing a new state. A
`running` snapshot therefore means the last observed state, not a health
check. Completed files are retained until manually removed, so results can be
inspected after a client exits.

Run the host-side checks without a VM, or the live checks against the configured
running image:

```sh
python3 Mezzano/tools/agent-harness/test_agent.py
python3 Mezzano/tools/agent-harness/test_live.py
```

The live checks evaluate code, create temporary job files, and remove successful
test jobs. Unexpectedly live or failed test jobs are left for inspection.
