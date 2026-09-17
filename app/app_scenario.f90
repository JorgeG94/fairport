! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Running a scenario script.
module app_scenario
   !! A session is a file. That is the point of the format.
   !!
   !! Because a scenario is plain text and the simulation is reproducible from
   !! `(seed, script)`, a regression test is
   !! `fairport scenarios/clear_day.txt --seed 42 --hash` compared against a
   !! committed string, and a bug report is the script that produced it.
   use core_sim, only: sim_t, tick_k, id_k, MILLISECOND, SECOND, MINUTE, HOUR, DAY, &
                       tick_split, &
                       command_t, command_kind_from_name, command_arg_count, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, &
                       PHASE_APPROACH, CALLSIGN_LEN
   use app_airport, only: load_airport
   use app_schedule, only: load_schedule, add_arrival, parse_clock_text, wake_from_letter
   use app_render, only: render_board, render_stats
   use app_text, only: int_text
   use pic_types, only: default_int, int16, int32, int64
   use pic_error, only: error_t, error_raise, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION
   use pic_string_type, only: string_type, char
   use pic_tokenizer, only: tokenize, parse_int
   implicit none
   private

   public :: run_scenario
   public :: load_scenario
   public :: parse_command
   public :: session_t
   public :: save_session

   integer, parameter :: MAX_LINE = 512
      !! Longest input line the parser accepts.
   integer(default_int), parameter :: DEFAULT_MAX_AIRCRAFT = 64_default_int
      !! Aircraft slots reserved when a scenario does not say.

   integer, parameter :: MAX_PREAMBLE = 128
      !! Directive lines a session can carry into its save file. A scenario is
      !! a handful of `load` and `arrival` lines; running out means the file
      !! was not the kind of thing this format is for, so it is an error rather
      !! than a silent truncation that would replay as a different day.

   type :: session_t
      !! Everything a saved session needs that the command log does not hold.
      !!
      !! The log is the player. This is the world the player was playing: which
      !! airport, which schedule, how long the day ran. Together with
      !! `sim%seed` that is a complete session, which is the invariant stated
      !! the other way round.
      character(len=MAX_LINE) :: preamble(MAX_PREAMBLE) = ""
         !! The world-describing lines, verbatim and comment-stripped.
      integer(default_int) :: n_preamble = 0_default_int
         !! How many of them there are.
      integer(tick_k) :: horizon = 24_tick_k*HOUR
         !! Where the day stops.
   end type session_t

   type :: scenario_t
      !! What one pass over the script has established so far.
      integer(int64) :: seed = 0_int64
      logical :: seed_from_cli = .false.
      integer(default_int) :: max_aircraft = DEFAULT_MAX_AIRCRAFT
      logical :: airport_loaded = .false.
      logical :: want_board = .false.
      logical :: want_stats = .false.
   end type scenario_t

contains

   subroutine run_scenario(sim, path, seed_override, has_seed_override, want_board, err, session)
      !! Read a scenario file and run it to completion.
      type(sim_t), target, intent(inout) :: sim
         !! Simulation to drive. Must be a target: the bus points into it.
      character(len=*), intent(in) :: path
         !! Path to the scenario script.
      integer(int64), intent(in) :: seed_override
         !! Seed from the command line, used when `has_seed_override`.
      logical, intent(in) :: has_seed_override
         !! Whether the command line supplied a seed.
      logical, intent(in) :: want_board
         !! Whether to draw the ops board at the end regardless of the script.
      type(error_t), intent(inout), optional :: err
      type(session_t), intent(out), optional :: session
         !! The world half of the script, kept so the run can be saved.

      type(scenario_t) :: scenario
      character(len=MAX_LINE) :: line
      type(string_type), allocatable :: tokens(:)
      integer :: unit, status
      integer(default_int) :: line_no

      scenario%seed = seed_override
      scenario%seed_from_cli = has_seed_override
      scenario%want_board = want_board

      open (newunit=unit, file=path, status="old", action="read", iostat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_IO, "run_scenario: cannot open "//trim(path))
         return
      end if

      line_no = 0_default_int
      do
         read (unit, "(a)", iostat=status) line
         if (status /= 0) exit
         line_no = line_no + 1_default_int

         call strip_comment(line)
         tokens = tokenize(trim(line))
         if (size(tokens) == 0) cycle

         call remember(session, tokens, line, line_no, err)
         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if

         call run_directive(sim, scenario, tokens, line_no, err)
         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if
      end do
      close (unit)

      if (scenario%want_board) call render_board(sim)
      if (scenario%want_stats) call render_stats(sim)
   end subroutine run_scenario

   subroutine load_scenario(sim, path, seed_override, has_seed_override, horizon, err, session)
      !! Set a day up without running any of it.
      !!
      !! Everything that describes the world -- seed, airport, schedule, the
      !! aircraft themselves -- is applied. What is skipped is time: `run
      !! until` becomes the horizon rather than a run, and `at` directives are
      !! dropped, because in an interactive session the player issues the
      !! commands rather than the file.
      type(sim_t), target, intent(inout) :: sim
         !! Simulation to populate.
      character(len=*), intent(in) :: path
         !! Scenario to load.
      integer(int64), intent(in) :: seed_override
         !! Seed from the command line.
      logical, intent(in) :: has_seed_override
         !! Whether the command line supplied one.
      integer(tick_k), intent(out) :: horizon
         !! Latest `run until` in the file, or the end of the day.
      type(error_t), intent(inout), optional :: err
      type(session_t), intent(out), optional :: session
         !! The world half of the script, kept so the session can be saved.

      type(scenario_t) :: scenario
      character(len=MAX_LINE) :: line
      type(string_type), allocatable :: tokens(:)
      integer(tick_k) :: at
      integer :: unit, status
      integer(default_int) :: line_no

      horizon = 24_tick_k*HOUR
      scenario%seed = seed_override
      scenario%seed_from_cli = has_seed_override

      open (newunit=unit, file=path, status="old", action="read", iostat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_IO, "load_scenario: cannot open "//trim(path))
         return
      end if

      line_no = 0_default_int
      do
         read (unit, "(a)", iostat=status) line
         if (status /= 0) exit
         line_no = line_no + 1_default_int

         call strip_comment(line)
         tokens = tokenize(trim(line))
         if (size(tokens) == 0) cycle

         call remember(session, tokens, line, line_no, err)
         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if

         select case (char(tokens(1)))
         case ("at")
            ! The player's job now, not the file's.
         case ("run")
            if (size(tokens) == 3) then
               call parse_clock(char(tokens(3)), at, line_no, err)
               horizon = at
            end if
         case ("board", "dump")
            ! Batch reporting; the board is live instead.
         case default
            call run_directive(sim, scenario, tokens, line_no, err)
         end select

         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if
      end do
      close (unit)
   end subroutine load_scenario

   subroutine run_directive(sim, scenario, tokens, line_no, err)
      !! Execute one line of the script.
      type(sim_t), target, intent(inout) :: sim
      type(scenario_t), intent(inout) :: scenario
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      select case (char(tokens(1)))
      case ("seed")
         call do_seed(sim, scenario, tokens, line_no, err)
      case ("max_aircraft")
         call do_max_aircraft(scenario, tokens, line_no, err)
      case ("load")
         call do_load(sim, scenario, tokens, line_no, err)
      case ("arrival")
         call do_arrival(sim, tokens, line_no, err)
      case ("at")
         call do_at(sim, tokens, line_no, err)
      case ("run")
         call do_run(sim, tokens, line_no, err)
      case ("board")
         scenario%want_board = .true.
      case ("dump")
         scenario%want_stats = .true.
      case default
         call error_raise(err, ERROR_PARSE, "run_scenario: unknown directive '"// &
                          char(tokens(1))//"' on line "//int_text(int(line_no, int64)))
      end select
   end subroutine run_directive

   subroutine do_seed(sim, scenario, tokens, line_no, err)
      !! `seed <n>`. The command line wins if it supplied one.
      type(sim_t), target, intent(inout) :: sim
      type(scenario_t), intent(inout) :: scenario
      type(string_type), intent(in) :: tokens(:)
      integer(default_int), intent(in) :: line_no
      type(error_t), intent(inout), optional :: err

      integer(int64) :: value
      type(error_t) :: parse_err

      if (size(tokens) /= 2) then
         call error_raise(err, ERROR_PARSE, "run_scenario: seed needs one value on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_int(char(tokens(2)), value, parse_err)
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: seed is not an integer on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      if (.not. scenario%seed_from_cli) scenario%seed = value
      call sim%init(scenario%seed, err)
   end subroutine do_seed

   subroutine do_max_aircraft(scenario, tokens, line_no, err)
      !! `max_aircraft <n>`.
      type(scenario_t), intent(inout) :: scenario
      type(string_type), intent(in) :: tokens(:)
      integer(default_int), intent(in) :: line_no
      type(error_t), intent(inout), optional :: err

      integer(int32) :: value
      type(error_t) :: parse_err

      if (size(tokens) /= 2) then
         call error_raise(err, ERROR_PARSE, "run_scenario: max_aircraft needs one value on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_int(char(tokens(2)), value, parse_err)
      if (parse_err%has_error() .or. value <= 0_int32) then
         call error_raise(err, ERROR_PARSE, "run_scenario: max_aircraft must be a positive integer on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      scenario%max_aircraft = int(value, default_int)
   end subroutine do_max_aircraft

   subroutine do_load(sim, scenario, tokens, line_no, err)
      !! `load airport <path>`.
      type(sim_t), intent(inout) :: sim
      type(scenario_t), intent(inout) :: scenario
      type(string_type), intent(in) :: tokens(:)
      integer(default_int), intent(in) :: line_no
      type(error_t), intent(inout), optional :: err

      if (size(tokens) /= 3) then
         call error_raise(err, ERROR_PARSE, "run_scenario: load needs a kind and a path on line "// &
                          int_text(int(line_no, int64)))
         return
      end if
      select case (char(tokens(2)))
      case ("airport")
         call load_airport(sim, char(tokens(3)), scenario%max_aircraft, err)
         if (present(err)) then
            if (err%has_error()) return
         end if
         scenario%airport_loaded = .true.
      case ("schedule")
         if (.not. scenario%airport_loaded) then
            call error_raise(err, ERROR_VALIDATION, &
                             "run_scenario: load the airport before the schedule, line "// &
                             int_text(int(line_no, int64)))
            return
         end if
         call load_schedule(sim, char(tokens(3)), err)
      case default
         call error_raise(err, ERROR_PARSE, "run_scenario: can load an 'airport' or a 'schedule', line "// &
                          int_text(int(line_no, int64)))
      end select
   end subroutine do_load

   subroutine do_arrival(sim, tokens, line_no, err)
      !! `arrival <callsign> <wake> <pax> at <HH:MM[:SS]> runway <n>`
      type(sim_t), intent(inout) :: sim
      type(string_type), intent(in) :: tokens(:)
      integer(default_int), intent(in) :: line_no
      type(error_t), intent(inout), optional :: err

      integer(id_k) :: aircraft, runway
      integer(int32) :: pax, runway_index, wake
      integer(tick_k) :: at
      type(error_t) :: parse_err

      if (size(tokens) /= 8) then
         call error_raise(err, ERROR_PARSE, &
                          "run_scenario: arrival needs <callsign> <wake> <pax> at <time> runway <n>, line "// &
                          int_text(int(line_no, int64)))
         return
      end if
      if (char(tokens(5)) /= "at" .or. char(tokens(7)) /= "runway") then
         call error_raise(err, ERROR_PARSE, "run_scenario: malformed arrival on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_int(char(tokens(4)), pax, parse_err)
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: pax is not an integer on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_clock(char(tokens(6)), at, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call parse_int(char(tokens(8)), runway_index, parse_err)
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: runway is not an integer on line "// &
                          int_text(int(line_no, int64)))
         return
      end if
      runway = int(runway_index, id_k)
      if (runway < 1_id_k .or. runway > sim%world%n_runways) then
         call error_raise(err, ERROR_VALIDATION, "run_scenario: no such runway on line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      wake = wake_code(char(tokens(3)), line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if
      if (.not. sim%world%has_stand_for(wake)) then
         call error_raise(err, ERROR_VALIDATION, "run_scenario: no stand at this airport takes a '"// &
                          char(tokens(3))//"' aircraft, line "//int_text(int(line_no, int64)))
         return
      end if

      call sim%world%aircraft%add(aircraft, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      sim%world%callsign(aircraft) = char(tokens(2))
      sim%world%aircraft%wake(aircraft) = wake
      sim%world%aircraft%pax(aircraft) = pax
      sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
      sim%world%aircraft%node(aircraft) = sim%world%runway_threshold(runway)
      sim%world%aircraft%generation(aircraft) = 1_int32

      call sim%schedule_touchdown(aircraft, runway, at, err)
   end subroutine do_arrival

   subroutine do_at(sim, tokens, line_no, err)
      !! `at <HH:MM[:SS[.mmm]]> <command> <a> [b]`
      !!
      !! Runs the simulation forward to the stated time, then issues the
      !! command. That ordering is what makes a script mean what it reads:
      !! a command submitted before time had advanced would take effect at tick
      !! zero rather than when the player pressed the key.
      type(sim_t), intent(inout) :: sim
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      type(command_t) :: command
      integer(tick_k) :: at

      if (size(tokens) < 4) then
         call error_raise(err, ERROR_PARSE, &
                          "run_scenario: at needs <time> <command> <argument>, line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_clock(char(tokens(2)), at, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call parse_command(tokens, 3_default_int, line_no, command, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      ! Advance to the moment the command was issued, then issue it.
      call sim%run_until(at, err)
      if (present(err)) then
         if (err%has_error()) return
      end if
      call sim%submit(command, err)
   end subroutine do_at

   subroutine do_run(sim, tokens, line_no, err)
      !! `run until <HH:MM[:SS]>`
      type(sim_t), intent(inout) :: sim
      type(string_type), intent(in) :: tokens(:)
      integer(default_int), intent(in) :: line_no
      type(error_t), intent(inout), optional :: err

      integer(tick_k) :: horizon

      if (size(tokens) /= 3) then
         call error_raise(err, ERROR_PARSE, "run_scenario: run needs 'until <time>' on line "// &
                          int_text(int(line_no, int64)))
         return
      end if
      if (char(tokens(2)) /= "until") then
         call error_raise(err, ERROR_PARSE, "run_scenario: only 'run until' exists, line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call parse_clock(char(tokens(3)), horizon, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call sim%run_until(horizon, err)
   end subroutine do_run

   subroutine remember(session, tokens, line, line_no, err)
      !! Keep one script line for the save file, if it describes the world.
      !!
      !! Three directives are deliberately not kept. `seed` is written from
      !! `sim%seed`, which is what actually ran after any command-line
      !! override. `run` becomes the horizon. `at` is not kept because the
      !! command log already has it, at the tick it really happened rather than
      !! the tick the file asked for -- and keeping both would replay every
      !! scripted command twice.
      type(session_t), intent(inout), optional :: session
         !! Session being built, or absent when nobody wants to save.
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      character(len=*), intent(in) :: line
         !! The line itself, already stripped of its comment.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(tick_k) :: at

      if (.not. present(session)) return

      select case (char(tokens(1)))
      case ("at", "seed")
         ! Held elsewhere; see above.
      case ("run")
         if (size(tokens) == 3) then
            call parse_clock(char(tokens(3)), at, line_no, err)
            session%horizon = at
         end if
      case default
         if (session%n_preamble >= int(MAX_PREAMBLE, default_int)) then
            call error_raise(err, ERROR_VALIDATION, &
                             "run_scenario: more than "//int_text(int(MAX_PREAMBLE, int64))// &
                             " directives to save, line "//int_text(int(line_no, int64)))
            return
         end if
         session%n_preamble = session%n_preamble + 1_default_int
         session%preamble(session%n_preamble) = trim(line)
      end select
   end subroutine remember

   subroutine save_session(sim, session, path, err)
      !! Write a played session out as a scenario that replays it.
      !!
      !! This is the whole reason the command log exists in the shape it does.
      !! A day spent on the interactive board is otherwise unrepeatable: it
      !! happened, it produced a digest, and there is nothing to hand anybody.
      !! Written out, it is an ordinary scenario file -- same parser, same
      !! directives, no save-specific code path to drift -- so a session that
      !! went wrong is a bug report and a session that went right is a
      !! regression test.
      type(sim_t), intent(in) :: sim
         !! Simulation whose seed and command log are being saved.
      type(session_t), intent(in) :: session
         !! The world half, from the script that was loaded.
      character(len=*), intent(in) :: path
         !! File to write.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: hours, minutes, seconds, millis
      integer :: unit, status
      integer(default_int) :: i

      open (newunit=unit, file=path, status="replace", action="write", iostat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_IO, "save_session: cannot write "//trim(path))
         return
      end if

      ! No date and no wall-clock time in the header. A save file that differs
      ! run to run cannot be diffed against the one before it, and diffing two
      ! sessions is the first thing anyone does with them.
      write (unit, "(a)") "# A fairport session, written by --save."
      write (unit, "(a)") "#"
      write (unit, "(a)") "# Replay it exactly as it was played:"
      write (unit, "(a)") "#   fairport "//trim(path)//" --hash"
      write (unit, "(a)") ""
      write (unit, "(a,i0)") "seed ", sim%seed

      do i = 1_default_int, session%n_preamble
         write (unit, "(a)") trim(session%preamble(i))
      end do

      if (sim%world%commands%size() > 0_default_int) then
         write (unit, "(a)") ""
         call sim%world%commands%write_to(unit, err)
         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if
      end if

      call tick_split(session%horizon, hours, minutes, seconds, millis)
      ! `tick_split` wraps at midnight, which is right for a clock face and
      ! wrong for a horizon: a day ending at 24:00 would be written as 00:00
      ! and replay as an empty run.
      if (session%horizon >= DAY) hours = int(session%horizon/HOUR, int32)
      write (unit, "(a)") ""
      write (unit, "(a,i0.2,a,i0.2,a,i0.2,a,i0.3)") &
         "run until ", hours, ":", minutes, ":", seconds, ".", millis

      close (unit)
   end subroutine save_session

   subroutine parse_command(tokens, first, line_no, command, err)
      !! Parse `<name> <a> [b]` into a command.
      !!
      !! One parser, two front ends. A scenario's `at` line and a key typed
      !! into the interactive board reach this same routine, so a command
      !! cannot mean one thing in a script and another in the terminal -- which
      !! matters more than usual here, because a session saved from the board
      !! is replayed through the script path.
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      integer(default_int), intent(in) :: first
         !! Index of the command name within `tokens`.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics. Zero when there is no file.
      type(command_t), intent(out) :: command
         !! Command built from the line.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: expected, subject_a, subject_b
      type(error_t) :: parse_err

      if (int(size(tokens), default_int) < first) then
         call error_raise(err, ERROR_PARSE, "command: nothing to parse"//where_text(line_no))
         return
      end if

      command%kind = command_kind_from_name(char(tokens(first)))
      if (command%kind == 0_int16) then
         call error_raise(err, ERROR_PARSE, "command: unknown command '"// &
                          char(tokens(first))//"'"//where_text(line_no))
         return
      end if

      expected = command_arg_count(command%kind)
      if (int(size(tokens), default_int) /= first + int(expected, default_int)) then
         call error_raise(err, ERROR_PARSE, "command: "//char(tokens(first))// &
                          " takes "//int_text(int(expected, int64))//" argument(s)"//where_text(line_no))
         return
      end if

      subject_b = 0_int32
      call parse_int(char(tokens(first + 1_default_int)), subject_a, parse_err)
      if (.not. parse_err%has_error() .and. expected >= 2_int32) then
         call parse_int(char(tokens(first + 2_default_int)), subject_b, parse_err)
      end if
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "command: argument is not an integer"//where_text(line_no))
         return
      end if

      command%a = int(subject_a, id_k)
      command%b = int(subject_b, id_k)
   end subroutine parse_command

   pure function where_text(line_no) result(text)
      !! `, line N`, or nothing when the command did not come from a file.
      integer(default_int), intent(in) :: line_no
         !! Line number, or zero.
      character(len=:), allocatable :: text

      if (line_no <= 0_default_int) then
         text = ""
      else
         text = ", line "//int_text(int(line_no, int64))
      end if
   end function where_text

   subroutine parse_clock(text, tick, line_no, err)
      !! Parse `HH:MM`, `HH:MM:SS` or `HH:MM:SS.mmm` into a tick.
      !!
      !! The millisecond field exists because a saved session is written in
      !! this format and a command issued from the interactive board lands on
      !! whatever tick the frame was at. Rounding one to the nearest second
      !! would move it relative to the events around it, and the replay would
      !! diverge from the session it claims to reproduce.
      character(len=*), intent(in) :: text
         !! Clock text.
      integer(tick_k), intent(out) :: tick
         !! Milliseconds since midnight of the scenario day.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: hours, minutes, seconds, millis
      integer(default_int) :: first_colon, second_colon, point
      type(error_t) :: parse_err

      tick = 0_tick_k
      first_colon = index(text, ":")
      if (first_colon <= 1_default_int) then
         call error_raise(err, ERROR_PARSE, "run_scenario: '"//text// &
                          "' is not HH:MM or HH:MM:SS, line "//int_text(int(line_no, int64)))
         return
      end if

      seconds = 0_int32
      millis = 0_int32
      second_colon = index(text(first_colon + 1:), ":")

      call parse_int(text(1:first_colon - 1), hours, parse_err)
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: bad hour in '"//text// &
                          "', line "//int_text(int(line_no, int64)))
         return
      end if

      if (second_colon == 0_default_int) then
         call parse_int(text(first_colon + 1:), minutes, parse_err)
      else
         second_colon = first_colon + second_colon
         call parse_int(text(first_colon + 1:second_colon - 1), minutes, parse_err)
         if (.not. parse_err%has_error()) then
            point = index(text(second_colon + 1:), ".")
            if (point == 0_default_int) then
               call parse_int(text(second_colon + 1:), seconds, parse_err)
            else
               point = second_colon + point
               call parse_int(text(second_colon + 1:point - 1), seconds, parse_err)
               ! Written with exactly three digits, so it is read as a whole
               ! number of milliseconds rather than a fraction to be scaled.
               if (.not. parse_err%has_error()) then
                  if (len_trim(text(point + 1:)) /= 3_default_int) then
                     call error_raise(err, ERROR_PARSE, "run_scenario: '"//text// &
                                      "' needs exactly three digits after the point, line "// &
                                      int_text(int(line_no, int64)))
                     return
                  end if
                  call parse_int(text(point + 1:), millis, parse_err)
               end if
            end if
         end if
      end if
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: bad minute or second in '"//text// &
                          "', line "//int_text(int(line_no, int64)))
         return
      end if

      tick = int(hours, tick_k)*HOUR + int(minutes, tick_k)*MINUTE + &
             int(seconds, tick_k)*SECOND + int(millis, tick_k)*MILLISECOND
   end subroutine parse_clock

   function wake_code(text, line_no, err) result(code)
      !! Map a wake category letter to its code.
      character(len=*), intent(in) :: text
         !! One of L, M, H, S.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err
      integer(int32) :: code

      select case (text)
      case ("L", "l")
         code = WAKE_LIGHT
      case ("M", "m")
         code = WAKE_MEDIUM
      case ("H", "h")
         code = WAKE_HEAVY
      case ("S", "s")
         code = WAKE_SUPER
      case default
         code = WAKE_MEDIUM
         call error_raise(err, ERROR_PARSE, "run_scenario: unknown wake category '"//text// &
                          "' on line "//int_text(int(line_no, int64)))
      end select
   end function wake_code

   pure subroutine strip_comment(line)
      !! Blank everything from the first `#` onwards.
      character(len=*), intent(inout) :: line
         !! Line to strip, in place.

      integer(default_int) :: hash

      hash = index(line, "#")
      if (hash > 0_default_int) line(hash:) = " "
   end subroutine strip_comment

end module app_scenario
