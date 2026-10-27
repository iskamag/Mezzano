#!/usr/bin/env python3
"""Exercise the agent harness against the already-running Mezzano image."""
import json
from pathlib import Path
import shutil
import subprocess
import sys
import time
import unittest
import uuid

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
HOME = ROOT / "home"
CLI = [sys.executable, str(HERE / "agent.py")]


def command(*args, input=None, acceptable=(0,)):
    proc = subprocess.run(CLI + list(args), input=input, text=True, capture_output=True, timeout=15)
    if proc.returncode not in acceptable:
        raise AssertionError(f"{args}: exit {proc.returncode}\n{proc.stdout}\n{proc.stderr}")
    return proc.stdout


def request(*args, **kwargs):
    return json.loads(command(*args, **kwargs))


def await_state(job_id, states, timeout=8, newer_than=None):
    deadline = time.monotonic() + timeout
    while True:
        state = request("show", job_id)
        if state["state"] in states and (newer_than is None or state.get("generation", 0) > newer_than):
            return state
        if time.monotonic() >= deadline:
            raise AssertionError(f"timed out: {state}")
        time.sleep(0.1)


class LiveAgentTests(unittest.TestCase):
    jobs = []

    def eval(self, source, wait=5):
        state = request("eval", "--stdin", "--wait", str(wait), input=source, acceptable=(0, 1))
        self.jobs.append(state["id"])
        return state

    def control(self, kind, job_id, *args, expected="sent"):
        response = request(kind, job_id, *args)
        deadline = time.monotonic() + 8
        while True:
            status = request("control", job_id, response["control_id"])
            if status["state"] in {"sent", "error", "unknown"}:
                self.assertEqual(status["state"], expected, status)
                return status
            if time.monotonic() >= deadline:
                self.fail(f"control timed out: {status}")
            time.sleep(0.1)

    def test_real_reader_multiple_forms_and_shared_definitions(self):
        symbol = "*AGENT-TEST-" + uuid.uuid4().hex.upper() + "*"
        source = (f'(defparameter {symbol} 41)\n'
                  "#| ( #| ) |# nested |#\n"
                  f'(list {symbol} #\\) (symbol-name \'|a b|) "Zażółć")\n')
        result = self.eval(source)
        self.assertEqual(result["state"], "ok", result)
        self.assertIn("41", result["values"])
        self.assertIn('"a b"', result["values"])
        self.assertIn("Zażółć", result["values"])
        other = self.eval(f"(+ {symbol} 1)")
        self.assertEqual((other["state"], other.get("values")), ("ok", "42\n"))
        changed = self.eval(f"(setf {symbol} 99)")
        self.assertEqual(changed["state"], "ok", changed)
        # A repeated dispatch of the same ID must not run its source twice.
        command("_dispatch", result["id"], "--home", str(HOME), "--serial", str(ROOT / "serial.sock"),
                "--action", "submit", "--control-id", "", "--timeout", "10")
        duplicate = request("show", result["id"])
        self.assertEqual(duplicate["dispatch"]["reply"], "DUPLICATE", duplicate)
        unchanged = self.eval(symbol)
        self.assertEqual((unchanged["state"], unchanged.get("values")), ("ok", "99\n"))
        package = self.eval('(in-package :mezzano.internals)\n(package-name *package*)')
        self.assertEqual((package["state"], package.get("values")), ("ok", '"MEZZANO.INTERNALS"\n'))
        cleanup = self.eval(f"(progn (makunbound '{symbol}) (unintern '{symbol} :cl-user))")
        self.assertEqual(cleanup["state"], "ok", cleanup)

    def test_long_job_does_not_block_other_jobs(self):
        start = time.monotonic()
        slow = self.eval('(progn (write-line "early") (finish-output) (sleep 4) :slow)', wait=0)
        self.assertLess(time.monotonic() - start, 2)
        slow_id = slow["id"]
        await_state(slow_id, {"running"})
        quick = self.eval("(values 7 :ok)")
        self.assertEqual((quick["state"], quick.get("values")), ("ok", "7\n:OK\n"))
        self.assertEqual(request("show", slow_id)["state"], "running")
        self.assertIn("early", command("output", slow_id))
        timed = request("wait", slow_id, "--timeout", "0.1", acceptable=(124,))
        self.assertEqual(timed["state"], "running")
        done = await_state(slow_id, {"ok"})
        self.assertEqual(done["values"], ":SLOW\n")

    def test_debugger_restart_and_abort(self):
        job = self.eval('(progn (cerror "Keep going" "stop here") :continued)')
        self.assertEqual(job["state"], "debug", job)
        self.assertIn("stop here", job["debug"])
        self.assertIn("0: CONTINUE", job["debug"])
        self.assertIn("Backtrace:\n0:", job["debug"])
        # A parked debugger must not prevent a new job from running.
        other = self.eval("(+ 10 20)")
        self.assertEqual(other["values"], "30\n")
        rejected = self.control("restart", job["id"], "0", "--generation",
                                str(job["generation"] + 1), expected="error")
        self.assertEqual(rejected["reply"], "STALE-REQUEST")
        self.control("restart", job["id"], "0", "--generation", str(job["generation"]))
        resumed = await_state(job["id"], {"ok", "failed"})
        self.assertEqual((resumed["state"], resumed.get("values")), ("ok", ":CONTINUED\n"))
        bad = self.eval('(error "expected failure")')
        self.assertEqual(bad["state"], "debug", bad)
        self.control("abort", bad["id"])
        self.assertEqual(await_state(bad["id"], {"aborted"})["state"], "aborted")
        handled = self.eval('(handler-case (error "handled") (error () :handled))')
        self.assertEqual((handled["state"], handled.get("values")), ("ok", ":HANDLED\n"))
        stopped = self.eval('(progn (break "pause") :after-break)')
        self.assertEqual(stopped["state"], "debug", stopped)
        self.assertIn("0: CONTINUE", stopped["debug"])
        self.control("restart", stopped["id"], "0", "--generation", str(stopped["generation"]))
        finished = await_state(stopped["id"], {"ok", "failed"})
        self.assertEqual((finished["state"], finished.get("values")), ("ok", ":AFTER-BREAK\n"))

    def test_input_and_eof(self):
        state = self.eval('(progn (format t "Name? ") (list (read-line) (read-char nil nil :eof)))')
        self.assertEqual(state["state"], "input", state)
        self.assertEqual(command("output", state["id"]), "Name? ")
        self.control("input", state["id"], "--text", "Zażółć", "--generation", str(state["generation"]))
        following = await_state(state["id"], {"input"}, newer_than=state["generation"])
        rejected = self.control("input", state["id"], "--eof", "--generation",
                                str(state["generation"]), expected="error")
        self.assertEqual(rejected["reply"], "STALE-REQUEST")
        self.control("input", state["id"], "--eof", "--generation", str(following["generation"]))
        done = await_state(state["id"], {"ok", "debug", "failed"})
        self.assertEqual(done["state"], "ok", done)
        self.assertIn('"Zażółć"', done["values"])
        self.assertIn(":EOF", done["values"])

    def test_cancel_running_job(self):
        state = self.eval("(progn (sleep 60) :unexpected)", wait=0)
        await_state(state["id"], {"running"})
        self.control("abort", state["id"])
        self.assertEqual(await_state(state["id"], {"cancelled", "aborted"})["state"], "cancelled")

    @classmethod
    def tearDownClass(cls):
        # Preserve any unexpectedly live/failed job for investigation.
        for job_id in cls.jobs:
            try:
                state = request("show", job_id)["state"]
                if state in {"ok", "aborted", "cancelled"}:
                    shutil.rmtree(HOME / ".mezzano-agent" / job_id)
            except Exception:
                pass


if __name__ == "__main__":
    unittest.main(verbosity=2)
