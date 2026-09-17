#!/bin/bash
#
# Regenerate the committed fypp sources under src/core/generated/.
#
# fypp runs at development time only, exactly as it does in pic: the generated
# .f90 files are committed, so building fairport needs neither fypp nor Python.
# check_generated.sh parses the `fypp <template> > <output>` and
# `cp <output> <dir>` lines below to learn which modules are generated and where
# they live, so adding a module here is picked up automatically. Keep those two
# line shapes intact, one command per line.
#
# Redirect stdout only. `>&` sends fypp's diagnostics into the generated file,
# so a template error becomes a committed .f90 containing a traceback and no
# message on the terminal -- which is how it presents, silently.

set -euo pipefail
cd "$(dirname "$0")"

# generate
fypp core_event_kinds.fypp > core_event_kinds.f90
fypp core_aircraft.fypp > core_aircraft.f90
fypp core_command.fypp > core_command.f90

# copy
cp core_event_kinds.f90 ../../src/core/generated/
cp core_aircraft.f90 ../../src/core/generated/
cp core_command.f90 ../../src/core/generated/

# cleanup
rm -f core_event_kinds.f90 core_aircraft.f90 core_command.f90

echo "regenerated: src/core/generated/{core_event_kinds,core_aircraft,core_command}.f90"
