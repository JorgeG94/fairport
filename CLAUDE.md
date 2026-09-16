# fairport

A deterministic, event-driven airport operations simulator in Fortran 2018,
built on [pic](https://github.com/JorgeG94/pic) as its standard library.

## The one invariant

**The simulation is reproducible from `(seed, command log)` alone.** Every rule
below exists to protect that. If a change would break it, it is not a trade-off
to be weighed, it is a different program.

## Build

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
```

pic is fetched and pinned to a commit in the top-level `CMakeLists.txt`. The pin
is a SHA, not a tag: a tag can move, and a moved tag changes the event-log digest
under a build that looks unchanged. Bumping pic means editing `FAIRPORT_PIC_TAG`
and re-recording any golden digest that moves.

To develop against a local pic tree, configure with
`-DFETCHCONTENT_SOURCE_DIR_PIC=/path/to/pic`. That is FetchContent's own
override, so there is no second code path in the build to drift.

## Tooling

A local virtualenv holds everything that is not the compiler. It is gitignored;
recreate it with:

```bash
python3 -m venv .venv
.venv/bin/pip install fprettify fortitude-lint pre-commit cmake-format fypp
.venv/bin/pre-commit install
```

- **fortitude** lints, configured in `fortitude.toml`. The rule set is pic's,
  with `PORT` added and `C182` ignored; both differences are argued in the file.
  Run it with `.venv/bin/fortitude check`.
- **fprettify** formats. `src/core/generated/` is excluded, because
  `check_generated.sh` asserts the committed `.f90` is byte-for-byte what fypp
  produced -- reformatting it would fail that check, and the fix would be undone
  by the next `autogen.sh`. Formatting of generated code belongs in the template.
- **fypp** is only needed to re-run `tools/autogen/autogen.sh`. Without it
  `check_generated.sh` skips itself with a message, which means that test passes
  without verifying anything -- so keep it installed.

`.venv/bin/pre-commit run --all-files` runs the lot. The hooks use
`language: python` rather than pic's `language: system`, so they build their own
environments and work on a fresh clone with no venv activated.

## Layering

```
src/core/   the entire simulation -- no console I/O, no reals, no wall clock
app/    terminal, parsing, I/O  -- may use core_sim and nothing else
```

`tools/check_layering.sh` enforces this and runs as a test. It is not advisory.

## Rules specific to this project

These are on top of pic's `FORTRAN_STYLE.md` and `CLAUDE.md`, which apply in
full. Where they conflict, the reason is stated.

### Fixed-width kinds in canonical state

pic's rule is `integer(default_int)` everywhere. fairport's core uses the
fixed-width kinds in `core_kinds` -- `tick_k` for sim time, `id_k` for handles
-- because `default_int` changes width with `PIC_DEFAULT_INT8`, and a tick or a
handle that changes width changes what a saved command log means. Sizes, counts
and loop indices are local bookkeeping and stay `default_int`.

### No reals in `core/`

Positions are integer centimetres, money integer cents, time integer
milliseconds. Reals are allowed only in the query layer, which feeds
presentation and never feeds back.

### No list-directed output, anywhere output is compared

Field widths, spacing and exponent digits for `write(*,*)` are
processor-dependent. gfortran and ifx disagree, and the resulting golden-file
failure looks like whitespace noise. `app/app_text.f90` renders every number
with arithmetic and `achar`, and nothing else should render one at all.

### Event kinds are append-only

`tools/autogen/core_event_kinds.fypp` holds the canonical list. Renumbering an
entry does not break a build, it silently decodes every saved log as something
else. Add at the end of a section.

### Systems write their own fields, and talk by scheduling

A system may read anything in the world. It writes only the fields its module
header claims, and anything it wants another system to act on it says by
scheduling an event. Direct cross-system writes make execution order depend on
subscription order, which depends on whatever was last edited.

### Generated code

`tools/autogen/*.fypp` generates into `src/core/generated/`, which holds
generated code and nothing else, and the output is committed, so
building needs neither fypp nor Python. Edit the template, run
`tools/autogen/autogen.sh`, commit both. `check_generated.sh` runs as a test.

## Testing

test-drive, same pattern as pic. To add a suite: write
`test/test_<name>.f90` with a `collect_<name>_tests` subroutine, add `"<name>"`
to the `tests` list in `test/CMakeLists.txt`, and add the `use` and
`new_testsuite` lines to `test/main_tests.f90`, bumping the `allocate` count.

Run one suite with `./build/fairport-tester core_graph`, one test with
`./build/fairport-tester core_graph shortest_path_direct`.

Nine unit suites plus four end-to-end checks run under ctest: the scenario
runs, the digest repeats, the layering rules hold, the generated sources are
current, and the whole console output matches `test/golden/`.

The determinism tests are the ones that matter. Local checks cover "same seed,
same digest", "a different seed changes something", and agreement across
`PIC_DEFAULT_INT8`; the cross-compiler fan-in -- four front ends agreeing on one
digest -- belongs in CI.

`core_aircraft` pins the serialized byte length. That is deliberate: the
checkpoint format must not vary with `default_int`, and pinning the length is
the only way to assert it from inside a single build. Change the number only
when the field list changes, which retires old checkpoints anyway.

To regenerate the golden output after an intended change:

```bash
./build/fairport scenarios/clear_day.txt --seed 42 > test/golden/clear_day_seed42.txt
```

`.gitattributes` keeps golden files, scenarios and airports out of git's line
ending conversion. A CRLF rewrite there is a Windows-only diff that reads as
whitespace noise.

## Where milestone 0 stops

Implemented: scheduler, bus, world, graph loader, integer routing, batch mode,
the arrival chain from touchdown to on-blocks, the event-log digest.

Not yet, and deliberately: player commands, departures, the reservation table,
weather and ops policy, economy, passengers. `sim_schedule_touchdown` is the
only way into the queue and milestone 1 replaces it with a schedule loader and
commands, both arriving as events stamped for a tick rather than as direct
mutations.
