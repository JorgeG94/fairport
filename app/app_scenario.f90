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
   use core_sim, only: sim_t, tick_k, id_k, SECOND, MINUTE, HOUR, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, &
                       PHASE_APPROACH, CALLSIGN_LEN
   use app_airport, only: load_airport
   use app_render, only: render_board, render_stats
   use app_text, only: int_text
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, error_raise, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION
   use pic_string_type, only: string_type, char
   use pic_tokenizer, only: tokenize, parse_int
   implicit none
   private

   public :: run_scenario

   integer, parameter :: MAX_LINE = 512
      !! Longest input line the parser accepts.
   integer(default_int), parameter :: DEFAULT_MAX_AIRCRAFT = 64_default_int
      !! Aircraft slots reserved when a scenario does not say.

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

   subroutine run_scenario(sim, path, seed_override, has_seed_override, want_board, err)
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
      if (char(tokens(2)) /= "airport") then
         call error_raise(err, ERROR_PARSE, "run_scenario: only 'load airport' exists at milestone 0, line "// &
                          int_text(int(line_no, int64)))
         return
      end if

      call load_airport(sim, char(tokens(3)), scenario%max_aircraft, err)
      if (present(err)) then
         if (err%has_error()) return
      end if
      scenario%airport_loaded = .true.
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

   subroutine parse_clock(text, tick, line_no, err)
      !! Parse `HH:MM` or `HH:MM:SS` into a tick.
      character(len=*), intent(in) :: text
         !! Clock text.
      integer(tick_k), intent(out) :: tick
         !! Milliseconds since midnight of the scenario day.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: hours, minutes, seconds
      integer(default_int) :: first_colon, second_colon
      type(error_t) :: parse_err

      tick = 0_tick_k
      first_colon = index(text, ":")
      if (first_colon <= 1_default_int) then
         call error_raise(err, ERROR_PARSE, "run_scenario: '"//text// &
                          "' is not HH:MM or HH:MM:SS, line "//int_text(int(line_no, int64)))
         return
      end if

      seconds = 0_int32
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
            call parse_int(text(second_colon + 1:), seconds, parse_err)
         end if
      end if
      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "run_scenario: bad minute or second in '"//text// &
                          "', line "//int_text(int(line_no, int64)))
         return
      end if

      tick = int(hours, tick_k)*HOUR + int(minutes, tick_k)*MINUTE + int(seconds, tick_k)*SECOND
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
