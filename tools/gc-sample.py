"""Sample x86-64 guest PCs/stacks through QEMU's GDB stub.

Run from MBuild with GC_MAP=home/gc-live.map GC_FLAG=home/gc-flag.txt
GC_OUTPUT=/tmp/gc-samples.json GC_SAMPLES=3000 in the environment, then:
  gdb -q -nx -batch -ex 'set architecture i386:x86-64' \
      -ex 'target remote 127.0.0.1:12345' \
      -ex 'source Mezzano/tools/gc-sample.py' > /tmp/gc-gdb.log 2>&1

GC_MAP is generated with tools/generate-bochs-map-file.lisp in the guest.
GC_FLAG contains two decimal integers: the address of *GC-IN-PROGRESS*'s
value-cell value slot and the tagged address of NIL. Refresh the map after
hot-loading functions, before starting GDB. Run profiling separately from
performance trials: stopping CPUs disturbs timings. Frame walking is bounded
and approximate; raw PCs are retained for later symbolization.
"""
import bisect
import collections
import json
import os
import random
import signal
import threading

import gdb

symbols = []
with open(os.environ.get("GC_MAP", "home/gc-live.map")) as stream:
    for line in stream:
        parts = line.strip().split(" ", 1)
        try:
            symbols.append((int(parts[0], 16), parts[1]))
        except (ValueError, IndexError):
            pass
symbols.sort()
addresses = [address for address, _ in symbols]
with open(os.environ.get("GC_FLAG", "home/gc-flag.txt")) as stream:
    flag_address, nil_address = map(int, stream.read().split())


def name(pc):
    index = bisect.bisect_right(addresses, pc) - 1
    if index >= 0 and pc - addresses[index] < 65536:
        return symbols[index][1]
    return hex(pc)


def word(address):
    return int.from_bytes(gdb.selected_inferior().read_memory(address, 8), "little")


samples = []
try:
    for sample in range(int(os.environ.get("GC_SAMPLES", "3000"))):
        timer = threading.Timer(random.uniform(.015, .035),
                                lambda: os.kill(os.getpid(), signal.SIGINT))
        timer.start()
        try:
            gdb.execute("continue", to_string=True)
        finally:
            timer.cancel()
            timer.join()
        gc_active = word(flag_address) != nil_address
        for thread in gdb.selected_inferior().threads():
            thread.switch()
            pc = int(gdb.parse_and_eval("$pc"))
            fp = int(gdb.parse_and_eval("$rbp"))
            stack = [pc]
            for _ in range(15):
                if not 0x200000000000 <= fp < 0x400000000000 or fp % 8:
                    break
                try:
                    following, return_address = word(fp), word(fp + 8)
                except gdb.MemoryError:
                    break
                stack.append(return_address)
                if following <= fp or following - fp > 0x100000:
                    break
                fp = following
            samples.append(dict(sample=sample, cpu=thread.num, gc_active=gc_active,
                                pc=pc, stack=stack, names=[name(p) for p in stack]))
finally:
    with open(os.environ.get("GC_OUTPUT", "/tmp/gc-samples.json"), "w") as stream:
        json.dump(samples, stream)
    gdb.execute("detach")

active = [s for s in samples if s["gc_active"]
          and "%WAIT-FOR-INTERRUPT" not in s["names"][0]]
print("GC active stops:", len({s["sample"] for s in samples if s["gc_active"]}))
print("Active CPU samples during GC:", len(active))
for function, count in collections.Counter(s["names"][0] for s in active).most_common(40):
    print(count, function)
