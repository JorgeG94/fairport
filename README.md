# fairport

A deterministic, event-driven simulation of airport operations, in Fortran 2018.

The design goal that drives every decision: **the simulation is reproducible
from `(seed, command log)` alone.** Save files, replays, regression tests,
cross-platform CI and reproducible bug reports all fall out of that one
property for free. Give it up and you pay for each of them separately, badly.

Status: **milestone 0**. One aircraft lands, taxis and parks; four of them do it
without tripping over each other. No player input yet, no economy, no
passengers.

The same scenario produces the same event-log digest under Debug and Release,
under 4-byte and 8-byte default integers, and the whole console output is
compared against a committed golden file.

## Build

fairport uses [pic](https://github.com/JorgeG94/pic) as its standard library.
It is fetched and pinned to a commit, so a clean clone builds with nothing else
installed.

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
```

The pin is a SHA rather than a tag, because a tag can move and a moved tag would
change the event-log digest under a build that looks unchanged. To work against
a local pic checkout — which is what you want when changing both at once:

```bash
cmake -B build -DFETCHCONTENT_SOURCE_DIR_PIC=/path/to/pic
```

## Run

```bash
./build/fairport scenarios/clear_day.txt --seed 42 --board
```

```
 FAIRPORT            09:00:00     VIS 10000m   ARR 30/hr   DEP 30/hr

 #  CALL      TYPE  PHASE      NODE  GATE  TD        ONBLK     TAXI
  1  QFA412    H     at_gate      10    A2  06:45:00  06:47:49  2m49s
  2  VOZ881    M     at_gate      11    A3  06:52:00  06:54:19  2m19s
  3  UAE414    S     at_gate       9    A1  06:58:00  07:00:43  2m43s
  4  JST615    M     at_gate      12    B1  07:04:00  07:06:53  2m53s

 GATES  A1[UAE414] A2[QFA412] A3[VOZ881] B1[JST615] B2[------] B3[------]
```

`--hash` prints the event-log digest and nothing else, which is what the
cross-compiler CI job compares:

```bash
./build/fairport scenarios/clear_day.txt --seed 42 --hash
```

## Layout

```
src/core/        the entire simulation. No console I/O, no reals, no wall clock
  generated/     fypp output, committed; never edited by hand
  systems/       arrival manager, gates, taxi
  core_sim.f90   THE ONLY MODULE app/ MAY USE
app/             terminal, scenario parsing, file loading
tools/autogen/   fypp templates; the generated .f90 is committed
data/airports/   airport topology
scenarios/       scripts, which are also the regression tests
test/            unit suites, plus the end-to-end determinism check
```

Three rules are enforced by `tools/check_layering.sh`, which runs as a test:

- `core/` performs no console I/O and opens no files
- `app/` uses `core_sim` and nothing else from `core/`
- `core/` declares no reals, and never reaches for wall time or a
  non-reproducible deviate

The second is the one that matters most. `core_sim` is the façade a graphical
client will eventually be written against, and every direct reach past it now is
a line that client would have to reimplement.

## Contributing

```bash
python3 -m venv .venv
.venv/bin/pip install fprettify fortitude-lint pre-commit cmake-format fypp
.venv/bin/pre-commit install
```

`fortitude.toml` holds the lint rules. `.venv/bin/pre-commit run --all-files`
checks formatting, linting and the usual whitespace hygiene across the tree.

## Regenerating

fypp runs at development time only, exactly as it does in pic: the generated
`.f90` is committed, so building fairport needs neither fypp nor Python.

```bash
python3 -m pip install fypp
tools/autogen/autogen.sh
```

`tools/autogen/check_generated.sh` runs as a test and fails if a template was
edited without rerunning that.
