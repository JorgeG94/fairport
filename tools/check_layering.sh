#!/bin/bash
#
# The layering rules, enforced mechanically rather than by discipline.
#
# Every one of these is a rule the design document states and that nobody will
# reliably remember at half past six on a Friday. Two of them are what keep a
# graphical client from being a rewrite; the third is what keeps the
# determinism guarantee true.

set -uo pipefail
cd "$(dirname "$0")/.."

status=0

report() {
  echo "LAYERING VIOLATION: $1"
  echo "$2"
  status=1
}

# Search src/core/ or app/ for a pattern, ignoring comment lines. Every rule below
# is about what the code does, and a rule that fires on the paragraph
# explaining the rule is a rule nobody keeps switched on.
scan() {
  grep -rnE "$2" "$1" 2> /dev/null | grep -vE ':[0-9]+: *!' || true
}

# 1. core/ does no console I/O and opens no files.
#
# `write (unit, ...)` where `unit` is a dummy argument is allowed: that is the
# serializer, which the design puts in core/ deliberately. What is banned is
# core/ deciding on its own to talk to a terminal or touch the filesystem.
hits=$(scan src/core/ '(^|[^a-z_])print[ *]|write *\( *\*|read *\( *\*|open *\(|close *\(')
if [ -n "$hits" ]; then
  report "core/ performs console I/O or opens files" "$hits"
fi

# 2. app/ uses core_sim and nothing else from core/.
#
# This is the one that matters most. core_sim is the façade; the day a
# graphical client arrives it will have exactly that surface to write against,
# and every direct reach into core_world or core_scheduler now is a line that
# client will have to reimplement.
hits=$(scan app/ '^ *use +core_' | grep -v 'use core_sim' || true)
if [ -n "$hits" ]; then
  report "app/ reaches into core/ past core_sim" "$hits"
fi

# 3. core/ never draws a non-reproducible random number, and never asks what
#    time it is in the real world.
#
# pic_random_dist_real goes through libm, whose last ulp differs between glibc,
# Intel's libimf and Apple's; one ulp is enough to change an inter-arrival time
# that is later rounded to a millisecond. Wall time is not sim time.
hits=$(scan src/core/ 'pic_random_dist_real|monotonic_ms|monotonic_us|now_local|now_utc|date_and_time|system_clock|cpu_time')
if [ -n "$hits" ]; then
  report "core/ reaches for wall time or a non-reproducible deviate" "$hits"
fi

# 4. core/ does not parse TOML.
#
# toml-f allocates, reads files and carries its own error type -- three things
# core/ does not do. The schedule is app/'s to read and core/'s to receive.
hits=$(scan src/core/ '^ *use +tomlf')
if [ -n "$hits" ]; then
  report "core/ uses the TOML parser" "$hits"
fi

# 5. core/ holds no reals in canonical state.
#
# Positions are integer centimetres, money is integer cents, time is integer
# milliseconds. A real that reaches simulation state is a determinism bug
# waiting for a different optimisation level.
hits=$(scan src/core/ '^ *(real|double precision)')
if [ -n "$hits" ]; then
  report "core/ declares a real" "$hits"
fi

if [ "$status" -eq 0 ]; then
  echo "layering: all rules hold"
fi
exit "$status"
