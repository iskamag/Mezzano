#!/bin/sh
# Private bounded native proof for normal entry plus deliberate failed worker.
set -eu
repo=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$repo"
: "${MEZZANO_TEST_DEPS:?Set MEZZANO_TEST_DEPS to the native build dependencies}"
MEZZANO_TEST_OUTPUT=${MEZZANO_TEST_OUTPUT:-/tmp/mezzano-world-stop-native/}
export MEZZANO_TEST_OUTPUT
mkdir -p "$MEZZANO_TEST_OUTPUT"
sbcl --dynamic-space-size 4096 --no-sysinit --no-userinit \
  --script tools/tests/world-stop-image.lisp > "$MEZZANO_TEST_OUTPUT/build.log" 2>&1
status=0
timeout --signal=TERM 75 qemu-system-x86_64 -enable-kvm -machine pc -m 3G -smp 2 \
  -drive "file=$MEZZANO_TEST_OUTPUT/native.image,format=raw,if=ide" -snapshot \
  -serial "file:$MEZZANO_TEST_OUTPUT/serial.log" -monitor none -display none \
  -vga std -net none -no-reboot > "$MEZZANO_TEST_OUTPUT/qemu.log" 2>&1 || status=$?
if [ "$status" -ne 0 ] && [ "$status" -ne 124 ]; then
  echo "Native QEMU failed with status $status" >&2
  exit 1
fi
rg -q '^WORLD-STOP-NATIVE-NORMAL-PASSED' "$MEZZANO_TEST_OUTPUT/serial.log"
rg -q '^WORLD-STOP-NATIVE-FAILURE-ARMED' "$MEZZANO_TEST_OUTPUT/serial.log"
rg -q -- '----- PANIC -----' "$MEZZANO_TEST_OUTPUT/serial.log"
rg -q 'World-stopped operation did not complete!' "$MEZZANO_TEST_OUTPUT/serial.log"
if rg -q '^WORLD-STOP-NATIVE-ESCAPED' "$MEZZANO_TEST_OUTPUT/serial.log"; then
  echo 'Failed native worker escaped its fatal boundary' >&2
  exit 1
fi
echo 'WORLD-STOP-NATIVE-PASSED (private success and deliberate failure probe)'
