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

Nine unit suites plus the end-to-end checks run under ctest: the scenario runs,
the digest repeats, a saved session replays to the same digest and the same
report, the layering rules hold, the generated sources are current, and the
whole console output matches `test/golden/`.

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

### Commands are the only way player input enters

`sim%submit(command)` appends to `world%commands` and schedules the command's
own event kind for `now + 1`. Nothing else may mutate the world on the player's
behalf. `now + 1` rather than `now` so a command cannot join the batch already
being dispatched, where its effect would depend on how far through that batch
the queue had got.

Command identifiers occupy event kinds 60-79 and are declared in
`tools/autogen/commands.fypp`, which both `core_event_kinds.fypp` and
`core_command.fypp` include. One list, two consumers. `test_core_command`
asserts the two numberings never collide.

The scheduled event carries the command's log index as its payload, so a
handler recovers all four fields rather than the two an event has room for, and
what it acts on is exactly what was written down.

## Where milestone 1 has got to

Done:

- **Commands and the command log**, so `(seed, command log)` is complete rather
  than half-built. All eleven are handled; `assign_gate` was the first.
- **The TOML schedule loader**, via toml-f pinned at v0.5.2. `[[arrival]]` pins
  an exact movement; `[[bank]]` states volume and shape and the loader draws
  the movements from the schedule's own RNG stream. Pinned movements load
  first, so a command naming aircraft 1 keeps meaning the same aircraft when a
  bank is resized.

- **The departure manager.** Turnaround, pushback, stand release, taxi out,
  the runway queue and takeoff. `WAKE_SEP_SEC` finally has a caller, and
  `hold_departure` / `release_departure` are the player's lever on it.

- **Weather, capacity, holding and diversion.** `sys_ops_policy` turns
  visibility into two integers; arrivals that cannot be cleared hold, burn
  fuel, and divert at bingo. `scenarios/fog_bank.txt` is the design's own
  scenario: the same traffic as `full_day.txt` with two hours of fog diverts
  fourteen aircraft instead of none.

- **Sequencing.** `sequence_arrival` and `sequence_departure` reorder the
  landing stack and the takeoff queue. With holding scarce, this is how the
  player chooses *who* diverts.

- **`hold_arrival` / `release_arrival`**, the arrival half of a pair that was
  asymmetric for a milestone. `scenarios/held_too_long.txt` is what they cost.

- **The schedule at the design's scale.** Forty arrivals, which is eighty
  movements once each leaves again.

- **The reservation table** (`core_reservations`). A flat pool with an
  intrusive list per node and a free list through the same `next` array. No
  allocation after load, no generic container, and the iteration order over a
  node is a function of insertion order alone.

- **The interactive terminal** (`app_tui`), behind `-DFAIRPORT_ENABLE_TUI=ON`.
  A 2 Hz ops board on `pic_ansi`'s `frame_t`, driven by `pic_term`. The arrow
  keys move a cursor, `h`/`r`/`f` are shortcuts against the selected aircraft,
  and `:` takes any command in scenario syntax.

- **`--save`**, which writes a played session back out as a scenario file.

**That completes milestone 1.**

### A played session is a file, or it did not happen

`--save` writes `seed`, the world-describing directives verbatim, the command
log, and the horizon. That is a complete scenario, read back by the same parser
a hand-written one is, so there is no save-specific code path to drift. It is
what turns a day on the interactive board into something that can be handed to
somebody -- otherwise a session happens, produces a digest, and leaves nothing.

Three directives are not copied into the save. `seed` is written from
`sim%seed`, which is what actually ran after any `--seed` override. `run`
becomes the horizon. `at` is not copied because the command log already holds
it, at the tick it really happened rather than the tick the file asked for --
copying both would replay every scripted command twice.

`log_write_to` claimed all of this in its docstring for a whole milestone and
none of it was true: it wrote raw milliseconds, `parse_clock` only accepted
`HH:MM`, and the routine had no callers at all. A documented round trip with no
test is a comment.

### Saved times carry milliseconds, and the third digit is required

`HH:MM:SS.mmm`. A command issued from the board lands on whatever tick the
frame was at, and rounding it to the nearest second moves it relative to the
events around it: a 345 ms shift changes the digest of `costly_favour`. Every
scripted command in `scenarios/` sits at `.000`, so the ctest round trip would
pass vacuously on this point -- `a_saved_session_replays_to_the_same_digest`
exists to put a command on a tick that is not a whole second.

`.25` is rejected rather than read. It is a quarter of a second to a reader and
twenty-five milliseconds to a parser that reads an integer, and refusing it is
the only reading that cannot be silently wrong.

`tick_split` wraps at midnight, which is right for a clock face and wrong for a
save: a horizon of `24:00` would be written `00:00` and replay as an empty run.
The save format does its own arithmetic for that reason.

### One command parser, two front ends

`parse_command` takes the tokens of `<name> <a> [b]` and is called by both the
scenario's `at` directive and the terminal's `:` line. A command cannot mean
one thing in a script and another when typed, which matters here more than
usual because a session typed into the board is replayed through the script.

The board's one-key shortcuts go through `command_kind_from_name` rather than a
hard-coded identifier, so `h` cannot drift from `commands.fypp`.

### Redirect stdout only from a generator

`autogen.sh` used `>&`, which sends fypp's diagnostics into the generated file.
A template error became a committed `.f90` containing a traceback, with nothing
on the terminal to say so. It presents as the script producing no output at
all.

### The terminal changes nothing about the simulation

Sim time still advances only through `run_until`, commands still go through
`submit`, and the digest of a day watched interactively is the digest of the
same day in batch. The board is a second consumer of the query layer, exactly
as a graphical client would be. `--play` is refused outright without a terminal
rather than guessed at.

### Ask FetchContent where a dependency ended up

`${pic_BINARY_DIR}/modules`, never a guess at where pic keeps its `.mod` files.
pic v0.8.2 moved them from `${CMAKE_BINARY_DIR}/modules` to
`${PROJECT_BINARY_DIR}/modules` -- correct, since a dependency writing into its
parent's build tree collides the moment two of them do -- and every consumer
with the old spelling hardcoded stopped compiling.

### One `error_t` per call, never a shared one

`error_t` accumulates: a failure left in it is still there at the next check.
Reusing one variable across `term_size` and `term_raw_enter` made an
unavailable terminal size look like a failure to enter raw mode, and the board
refused to start. Cost half an hour to find because the symptom named the wrong
call.

### An aircraft commits to a route it can complete, or waits

`sys_taxi` checks every node on a planned route against the window it would
need, and either claims the whole thing or asks again in forty-five seconds.
Replanning around the conflict is deliberately not attempted: the taxi solver is
meant to be mediocre, and the player is the optimizer.

Windows are half-open. An aircraft leaving a node at the instant the next
arrives is a handover, not a conflict, and treating it as one deadlocks an apron
with a single entrance.

### Departure sequencing acts at pushback, not at the holding point

Reservations made this necessary and obvious: there is one taxiway, nobody
overtakes on it, so the order departures push is the order they take off
whatever the queue at the threshold looks like. That is also how ground control
really sequences departures. Held aircraft are skipped rather than waited for,
or holding the front of the queue would stop everything behind it.

### `size` of an unallocated array is undefined, not zero

Guarding the reservation pool with a bounds check alone segfaulted four test
fixtures that never sized it. An unsized pool now constrains nothing, which is
exactly the behaviour before reservations existed.

### "~80 movements" counts departures too

Forty arrivals is eighty movements, not eighty arrivals. Reading it the other
way put a hundred and sixty movements through a six-stand airport and diverted
twenty-four aircraft.

What matters more than the count is the shape. Forty arrivals spread evenly
across a day is a trickle six stands absorb without complaint, and nothing
interesting happens -- zero diversions. Concentrated into two banks it is a
wave, and four aircraft run out of holding fuel. The bank windows are the
tuning knob for how hard the day is, not the movement count.

### Holding an arrival has to be able to kill it

`hold_arrival` takes an aircraft out of the landing order and nothing else. It
stays in the stack, keeps flying circuits, keeps burning fuel, and diverts at
bingo like anybody else. A hold that could not cost anything would be a free
action, and a free action is not a decision -- the player would hold everything
and sort it out later.

`hold_ordered` is the arrival's own field, not `held`, which is
`sys_departure`'s. Two systems writing one field is the cross-system write the
layering rule forbids, and which one won would depend on bus order.

The command is refused for an aircraft that has already landed or diverted, so
the field keeps meaning "this is being held right now" -- which is what the
report reads when it decides whose diversion happened under a hold.

### "Held at bingo" is a fact, not a verdict

The report names diversions that happened while the aircraft was still held,
and deliberately does not claim the hold caused them. Holding `MOR014` on
`full_day` counts there although it was going to divert regardless: the
simulation does not run the day twice to find out, and a number that quietly
guessed would be worse than one that states what happened.

Holding an aircraft that was *not* going to divert reads unambiguously.
`hold_arrival 22` takes `full_day` from five losses to six, and the sixth is
the aircraft that was held -- a pure loss, because the stack was already longer
than the runway and the slot it gave up went to somebody who would have got one
anyway.

`hold_arrival 17` is the more interesting one and still loses five: `MOR013`
dies and `MOR014` lives. `MOR013` is the Super. That is the same trade
`costly_favour.txt` makes from the other direction -- there the player promotes
an aircraft and costs the Super its slot; here they hold the Super and cost it
directly. Two commands, opposite in shape, identical in outcome.

### A controller grants the slot; aircraft only ask

The first sequencing implementation made the asking aircraft defer when it was
not at the front. That wasted the clearance, because the front aircraft would
not ask again for a full holding circuit -- costing two extra diversions and
doubling the holding on a busy day. `sys_arrival` now hands the clearance to the
front of the stack directly. `a_clearance_is_never_wasted` asserts the property.

The order is: sequenced beats unsequenced, lower position beats higher, then
longest-waiting, then the handle. The longest-waiting rule is what stops the
unsequenced majority starving while the player attends to two aircraft.

### "Can it be taken" is asked per aircraft, never per request

The stand check used to be asked for whoever requested the clearance, which then
went to whoever was at the front -- so a free Medium stand could clear a Super.
An aircraft no stand on the field could ever take is refused outright, whatever
the apron buffer says.

### You can only sequence what is already in the queue

A slot cannot be kept warm for an aircraft that has not arrived. Three
sequencing tests initially failed for this reason, and the fixtures now close
the airport, or hold the departures, until everybody is actually waiting.

### Capacity is two integers, never a boolean

"Closed" is those integers at zero. Every partial state -- arrivals only,
departures only, low-visibility procedures -- falls out of the same arithmetic
with no extra branch. A rate becomes a minimum spacing, which a runway honours
*alongside* wake separation: they are independent constraints and a movement
waits for `max(separation, rate spacing)`. Both are integer; `(x*4)/5`, never
`x*0.8`.

### A queue belongs in the air, not on the taxiway

An arrival with nowhere to park is refused its clearance and holds, once more
than `APRON_BUFFER` aircraft are already on the ground waiting for a stand.
Without that bound the model quietly absorbed unlimited traffic: one run had an
A380 sitting on the runway exit for five hours, and the only visible symptom was
a "mean gate-in" of forty-four minutes that averaged a three-minute taxi with a
five-hour wait.

Holding costs fuel and can end in a diversion, so over-scheduling now produces a
hard failure you can see rather than an invisible ground queue. Fifty-two
movements against six stands diverts six aircraft on a clear day, which is
roughly what the design's milestone 1 success criterion asks for.

Taxi time and stand wait are reported separately, for the same reason: they are
different failures and their average means nothing.

### One scheduled event per aircraft, never a poll

Bingo fuel is scheduled once when an aircraft enters the hold, and landing
bumps its generation so the pending timer is tombstoned. Nothing polls fuel.
Diversions then emerge in fuel order on their own. `landing_cancels_the_bingo_timer`
is the test that would catch the missing increment -- without it an aircraft
parks, turns round, and diverts from its stand.

### `runway_free_at` only ever moves forward

Every claimant on a runway -- an arrival touching down, a departure reserving a
takeoff slot, a departure rolling -- writes `max(current, mine)`, never a flat
assignment. Writing it flat let an arrival clobber a reservation a departure had
already made, and a Light rolled fifteen seconds behind a Heavy where the matrix
demands three minutes. `test_sys_departure` checks every consecutive pair, which
is how that was found: it was one pair in forty-nine.

### Phases are not end states

An aircraft that parks now also leaves again, so asserting
`phase == PHASE_AT_GATE` at a horizon is asserting the run stopped at the right
moment rather than that anything happened. Assert the tick -- `on_blocks_tick > 0`
-- which records that it happened at all. Two tests had to be corrected for this
when the departure manager landed.

### A bank is generated traffic

Which makes "it loaded without complaining" no evidence at all. Two traps hit
while writing its tests, both worth remembering:

- Comparing `touchdown_tick` **before running the simulation** compares two sets
  of zeros, and every loader passes that. `test_app_schedule` now asserts the
  first tick is non-zero before comparing anything.
- A `sim_t` that has not been `init`-ed has an empty bus, so nothing dispatches
  at all. Seeding and initialising belong in the fixture, not in the test.

### toml-f is `app/`'s alone

It allocates, reads files and carries its own error type. `check_layering.sh`
refuses `use tomlf` anywhere under `src/core/`. Its `pos` arguments are default
`integer`, which stops matching `default_int` under `PIC_DEFAULT_INT8=ON`, so
`app_schedule` converts at the boundary once rather than breaking in the other
integer width.

## Where milestone 0 stopped

Scheduler, bus, world, graph loader, integer routing, batch mode, the arrival
chain from touchdown to on-blocks, the event-log digest.
