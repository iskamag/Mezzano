Virtual memory layout for the x86-64 target
========

0000000000000000 - 0000000000400000  Unmapped
0000000000400000 - 0000000080000000  Wired area
0000000080000000 - 0000004000000000  Unused
0000004000000000 - 0000006000000000  Card table (wired)
0000006000000000 - 0000007F80000000  Unused
0000007F80000000 - 0000008000000000  Wired function area
0000008000000000 - 0000008080000000  Function area
0000008080000000 - 0000010000000000  Unused
0000010000000000 - 0000100000000000  Pinned area
0000100000000000 - 0000120000000000  GC mark bits (one per 2 words)
0000120000000000 - 0000200000000000  Unused
0000200000000000 - 0000200080000000  Wired stack area
0000200080000000 - 0000204000000000  Unused (future expansion for wired stack area)
0000204000000000 - 0000208000000000  DMA buffer mapping area
0000208000000000 - 0000400000000000  Stack area
0000400000000000 - 0000480000000000  General young generation (semispace A)
0000480000000000 - 0000500000000000  General old generation (semispace A)
0000500000000000 - 0000580000000000  General young generation (semispace B)
0000580000000000 - 0000600000000000  General old generation (semispace B)
0000600000000000 - 0000680000000000  Cons young generation (semispace A)
0000680000000000 - 0000700000000000  Cons old generation (semispace A)
0000700000000000 - 0000680000000000  Cons young generation (semispace B)
0000780000000000 - 0000800000000000  Cons old generation (semispace B)
0000800000000000 - FFFF800000000000  x86-64 non-canonical region
FFFF800000000000 - FFFF808000000000  Linear map of physical memory
FFFF808000000000 - FFFF808100000000  Page frame information vector
FFFF808100000000 -10000000000000000  Unused

Everything below 0000008000000000 (512G) is assumed to be wired.

The function area
--------

The wired and normal function areas both begin at 0000008000000000 and are
expanded downwards for the wired area and upward for the normal area.
The total combined size of the function areas cannot exceed 2GB.
This allows wired & normal functions to reach each other through normal
direct calls, which have a maximum range of +/-2GB.
ARM64 branches have a limit of +/-128MB, so the total size of the function
area is a little more limited, but this still seems to be enough.

The base address is chosen so it stradles the wired & pinned zones.
This is not a specific requirement, but was done to minimize disruption
to code which assumes the location of wired memory.

The entire function area will eventually be subject to the full write barrier.
Supervisor code must not write to it.
NX will eventually be applied to all memory outside the function area, to
assist with catching wild jumps.

Mark bits
----------
Every two words of memory has a mark bit assigned to it so that objects
do not need to have a mark bit in their header.

Thread-local allocation buffers
----------
General objects and conses use separate per-thread bump/limit pairs. General
buffers contain byte offsets into the young generation, represented as fixnums.
Cons buffers contain absolute, untagged virtual addresses in unboxed 64-bit
slots, excluded from the thread's GC pointer layout. Both pairs initially
contain zero.

Cons buffers reserve 64 KiB (4096 conses) at a time. The slow path reserves a
buffer from the global cons bump pointer under the allocator mutex, with
footholds inhibited and pseudo-atomic execution covering both reservation and
publication. Failed reservations leave the global bump pointer unchanged.
The per-cons fast path is emitted directly in CONS on both architectures and
uses ordinary loads, arithmetic, and stores, without atomic read/modify/write
instructions. DO-CONS supplies the same operation to the slow path. Their GC
restart regions include loading the limit, reserving a slot, and constructing
a valid cons pointer. Until the bounds check succeeds, the bump stays an
aligned raw address which looks like a fixnum to the register walker, including
when it points just past an exhausted buffer. Only an in-bounds address receives
the cons tag. The current semispace is already part of the buffer address. The backing memory is zero-filled, so the cons is valid
before its CAR and CDR are initialized outside the restart region.
The x86-64 fast paths issue `PREFETCHNTA` at the next bump address, as the
general-object allocator does.

Before converting a full thread save to an interrupt frame for a foothold or
pager call, the supervisor also rewinds wired restart regions, locating
the function from its saved PC so older versions left in flight by runtime
recompilation are handled as well. This lets an injected function allocate conses without the interrupted
allocator later publishing a stale bump pointer. Ordinary thread preemption
needs no special action, since another thread cannot use this thread's buffer.

After every collection, while the world is still stopped, the collector
clears both general and cons buffer pairs on every live thread. Unused tails
are discarded rather than traced as roots. A resumed allocator reloads the
cleared limit and refills from the current young semispace. Conses explicitly
allocated in pinned or wired space continue to use the freelist allocator.

`tools/tests/cons-tlab-guest.lisp` checks exact exhaustion, refill values and
allocation accounting, concurrent allocating threads, parked-thread buffer
invalidation, allocating footholds, and live lists across minor and major collections. To build a
private test image, run from the Mezzano directory:

```
sbcl --non-interactive --load tools/tests/cons-tlab-image.lisp
timeout 120 qemu-system-x86_64 -enable-kvm -machine q35 -m 3G -smp 4 \
  -drive file=/tmp/mezzano-cons-tlab-test/native.image,format=raw -snapshot \
  -serial stdio -monitor none -display none -vga std -net none -no-reboot
```

Successful execution prints `CONS-TLAB-TEST-PASSED`; QEMU must then be stopped
by the timeout. Rebuild the normal cold image with `make cold-image` and run
the usual bounded `make kvm` check as well. Changes to the thread structure
require `make clean` before building.

For a single-thread comparison in a running guest, compile and load
`tools/tests/cons-tlab-shared-baseline.lisp` and
`tools/tests/cons-tlab-benchmark.lisp`, then call
`(mezzano.cons-tlab-benchmark:run)`. The baseline preserves the shared allocator
from `68b7e85c` under separate names. Both allocators coexist safely because
cons buffer refills reserve their global range with CAS. The CAS is confined
to the refill path.

The benchmark alternates shared/TLAB order for 11 pairs of 500000 `(cons nil nil)`
calls on one guest CPU, with 16 calls per loop iteration in both modes.
It prepares and touches the young cons area before timing;
normal CONS metering, buffer refills, and their mutex operations are included.
Each sample checks the returned cons, exact per-thread allocation bytes,
and absence of GC during timing. Compilation, explicit collection, page
preparation, warmup, and result validation are outside the timed interval.
It reports elapsed TSC ticks and the median paired speedup.

Allocator code can be recompiled in that same guest with `compile-file` and
loaded wired using `(with-open-file (in llf :element-type '(unsigned-byte 8))
(mezzano.internals::load-llf in t))`. Compile and load the runtime assembly
first, then `runtime/allocate.lisp`, `system/gc.lisp`, and the benchmark callers,
so callers incorporate the updated definitions and slot representations.
Changes to the thread layout still need
a fresh cold image. For an isolated early serial listener, build the private
image with `MEZZANO_CONS_REPL=1`; subsequent code changes use the live compiler.
