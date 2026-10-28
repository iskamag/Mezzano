"""GDB sampling for clos-compile.lisp; run separately from timing trials.

Start (clos-compile:compile-workload 100), then:
gdb -q -nx -batch -ex 'target remote :1234' \
    -ex 'source Mezzano/tools/tests/clos-compile-gdb.py'
Generate /tmp/clos-cost/live.map in the guest before starting the workload.
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
with open('/tmp/clos-cost/live.map') as stream:
    for line in stream:
        try:
            address, name = line.strip().split(' ', 1)
            symbols.append((int(address, 16), name))
        except ValueError:
            pass  # Printed function names can span multiple lines.
symbols.sort()
addresses = [a for a, _ in symbols]

def name(pc):
    i = bisect.bisect_right(addresses, pc) - 1
    return symbols[i][1] if i >= 0 and pc - addresses[i] < 65536 else hex(pc)

def word(address):
    return int.from_bytes(gdb.selected_inferior().read_memory(address, 8), 'little')

samples = []
try:
    for iteration in range(400):
        timer = threading.Timer(random.uniform(.01, .03),
                                lambda: os.kill(os.getpid(), signal.SIGINT))
        timer.start()
        try:
            gdb.execute('continue', to_string=True)
        finally:
            timer.cancel()
            timer.join()
        for thread in gdb.selected_inferior().threads():
            thread.switch()
            pc = int(gdb.parse_and_eval('$pc'))
            fp = int(gdb.parse_and_eval('$rbp'))
            stack = [pc]
            for _ in range(100):
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
            names = [name(p) for p in stack]
            if not any('CLOS-COMPILE:COMPILE-WORKLOAD' in n for n in names):
                continue
            samples.append(dict(iteration=iteration, cpu=thread.num, pc=pc,
                                stack=stack, names=names,
                                asm=gdb.execute('x/4i $pc', to_string=True)))
finally:
    with open('/tmp/clos-cost/gdb-samples.json', 'w') as stream:
        json.dump(samples, stream, indent=2)
    gdb.execute('detach')

print('Compilation CPU samples:', len(samples))
for function, count in collections.Counter(s['names'][0] for s in samples).most_common(35):
    print(count, function)
