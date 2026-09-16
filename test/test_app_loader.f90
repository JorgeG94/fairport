! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the airport loader and the scenario parser.
module test_app_loader
   !! The design document calls the tokenizer "the single least pleasant file
   !! in the project", and says to test it hard and then never look at it
   !! again. This is that. Every one of these tests is a malformed input
   !! someone will eventually type.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use app_airport, only: load_airport
   use app_scenario, only: run_scenario
   use core_sim, only: sim_t, id_k, tick_k, HOUR, MINUTE, &
                       WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, PHASE_AT_GATE
   use pic_error, only: error_t
   use pic_types, only: default_int, int32, int64
   implicit none
   private

   public :: collect_app_loader_tests

   character(len=*), parameter :: AIRPORT_PATH = "test_loader_airport.txt"
   character(len=*), parameter :: SCENARIO_PATH = "test_loader_scenario.txt"

contains

   subroutine collect_app_loader_tests(testsuite)
      !! Register the loader and parser tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("loads_a_well_formed_airport", test_loads_airport), &
                  new_unittest("comments_and_blank_lines_are_skipped", test_comments), &
                  new_unittest("unknown_directive_is_rejected", test_unknown_directive), &
                  new_unittest("wrong_field_count_is_rejected", test_field_count), &
                  new_unittest("non_integer_field_is_rejected", test_non_integer), &
                  new_unittest("unknown_wake_category_is_rejected", test_bad_wake), &
                  new_unittest("edge_outside_the_graph_is_rejected", test_bad_edge), &
                  new_unittest("missing_file_is_reported", test_missing_file), &
                  new_unittest("airport_with_no_runway_is_rejected", test_no_runway), &
                  new_unittest("scenario_runs_end_to_end", test_scenario_runs), &
                  new_unittest("scenario_clock_accepts_both_forms", test_clock_forms), &
                  new_unittest("scenario_rejects_a_bad_clock", test_bad_clock), &
                  new_unittest("scenario_rejects_an_unknown_runway", test_bad_runway), &
                  new_unittest("scenario_rejects_an_unstandable_aircraft", test_no_stand), &
                  new_unittest("command_line_seed_beats_the_script", test_seed_override) &
                  ]
   end subroutine collect_app_loader_tests

   subroutine write_file(path, lines)
      !! Write a scratch input file, one array element per line.
      character(len=*), intent(in) :: path
         !! File to write.
      character(len=*), intent(in) :: lines(:)
         !! Lines, written trimmed.

      integer :: unit, i

      open (newunit=unit, file=path, status="replace", action="write")
      do i = 1, size(lines)
         write (unit, "(a)") trim(lines(i))
      end do
      close (unit)
   end subroutine write_file

   subroutine discard(path)
      !! Remove a scratch file if it exists.
      character(len=*), intent(in) :: path
         !! File to delete.

      integer :: unit
      logical :: there

      inquire (file=path, exist=there)
      if (.not. there) return
      open (newunit=unit, file=path, status="old", action="read")
      close (unit, status="delete")
   end subroutine discard

   subroutine write_good_airport()
      !! Three nodes, one stand, one runway. The smallest workable airport.

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "name TINY", &
                                     "node runway_threshold 0 0 S", &
                                     "node runway_exit 1000 0 S", &
                                     "node gate 2000 0 H", &
                                     "edge 1 2 30000", &
                                     "edge 2 3 40000", &
                                     "gate A1 3 H", &
                                     "runway 09 1 2"])
   end subroutine write_good_airport

   subroutine test_loads_airport(error)
      !! A well-formed file produces the graph, stands and runways it names.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)
      call check(error,.not. err%has_error(), "a valid airport was rejected")
      if (allocated(error)) return

      call check(error, sim%world%graph%n_nodes == 3_int32, "node count")
      if (allocated(error)) return
      ! Two undirected edges are four directed ones.
      call check(error, sim%world%graph%n_edges == 4_int32, "edge count")
      if (allocated(error)) return
      call check(error, sim%world%n_gates == 1_int32, "gate count")
      if (allocated(error)) return
      call check(error, sim%world%n_runways == 1_int32, "runway count")
      if (allocated(error)) return
      call check(error, sim%world%gate_node(1) == 3_id_k, "gate node")
      if (allocated(error)) return
      call check(error, trim(sim%world%gate_name(1)) == "A1", "gate label")
      if (allocated(error)) return
      call check(error, sim%world%runway_threshold(1) == 1_id_k, "runway threshold")
      if (allocated(error)) return
      call check(error, sim%world%runway_exit(1) == 2_id_k, "runway exit")
      if (allocated(error)) return
      call check(error, sim%world%graph%x_cm(2) == 1000_int32, "node coordinate")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_loads_airport

   subroutine test_comments(error)
      !! Comments, indentation and blank lines carry no meaning.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "# a leading comment", &
                                     "", &
                                     "   name TINY   # trailing comment", &
                                     "node runway_threshold 0 0 S", &
                                     "   node runway_exit 1000 0 S", &
                                     "node gate 2000 0 H   ", &
                                     "edge 1 2 30000", &
                                     "edge 2 3 40000   # and here", &
                                     "", &
                                     "gate A1 3 H", &
                                     "runway 09 1 2", &
                                     "# a trailing comment"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error,.not. err%has_error(), "comments or blanks were not skipped")
      if (allocated(error)) return
      call check(error, sim%world%graph%n_nodes == 3_int32, "node count")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_comments

   subroutine test_unknown_directive(error)
      !! An unrecognised keyword is a parse error naming the line.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node runway_threshold 0 0 S", &
                                     "hangar 1 2 3"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "an unknown directive was accepted")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "hangar") > 0, "the message does not name the directive")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "line 2") > 0, "the message does not name the line")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_unknown_directive

   subroutine test_field_count(error)
      !! Too few fields is an error rather than a silent default.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node runway_threshold 0 0"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "a short node line was accepted")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_field_count

   subroutine test_non_integer(error)
      !! A coordinate that is not an integer is rejected, not truncated.
      !!
      !! `pic_tokenizer`'s `parse_int` is strict, so "10x" is an error rather
      !! than 10. That matters here: a silently truncated coordinate would
      !! produce a plausible airport that routes subtly wrongly.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node runway_threshold 10x 0 S"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "'10x' was accepted as an integer")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "10x") > 0, "the message does not quote the token")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_non_integer

   subroutine test_bad_wake(error)
      !! Wake categories are L, M, H, S and nothing else.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node runway_threshold 0 0 X"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "wake category 'X' was accepted")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bad_wake

   subroutine test_bad_edge(error)
      !! An edge to a node that does not exist is caught at build time.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node runway_threshold 0 0 S", &
                                     "node runway_exit 1000 0 S", &
                                     "edge 1 99 30000", &
                                     "runway 09 1 2"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "an edge to node 99 was accepted")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bad_edge

   subroutine test_missing_file(error)
      !! A missing file names itself.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call load_airport(sim, "no_such_airport_file.txt", 4_default_int, err)

      call check(error, err%has_error(), "a missing file was not reported")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "no_such_airport_file.txt") > 0, &
                 "the message does not name the file")

      call sim%destroy()
   end subroutine test_missing_file

   subroutine test_no_runway(error)
      !! An airport with no runway is rejected rather than loaded empty.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_file(AIRPORT_PATH, [character(len=64) :: &
                                     "node gate 0 0 H", &
                                     "node gate 100 0 H", &
                                     "edge 1 2 1000", &
                                     "gate A1 1 H"])
      call load_airport(sim, AIRPORT_PATH, 4_default_int, err)

      call check(error, err%has_error(), "an airport with no runway was accepted")

      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_no_runway

   subroutine test_scenario_runs(error)
      !! A whole scenario, from script to a parked aircraft.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 42", &
                                      "max_aircraft 4", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 M 100 at 06:00 runway 1", &
                                      "run until 09:00"])

      call run_scenario(sim, SCENARIO_PATH, 0_int64, .false., .false., err)
      call check(error,.not. err%has_error(), "the scenario reported an error")
      if (allocated(error)) return

      call check(error, sim%world%aircraft%size() == 1_default_int, "no aircraft was created")
      if (allocated(error)) return
      call check(error, trim(sim%world%callsign(1)) == "TEST01", "callsign")
      if (allocated(error)) return
      ! Not `phase == PHASE_AT_GATE`. Since the departure manager landed, an
      ! aircraft that parks also leaves again, so the end state is `departed`
      ! and the evidence that it parked is the tick, not the phase.
      call check(error, sim%world%aircraft%on_blocks_tick(1) > 0_tick_k, "the aircraft never parked")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) == 6_tick_k*HOUR, "touchdown time")
      if (allocated(error)) return
      call check(error, sim%world%now == 9_tick_k*HOUR, "the horizon was not reached")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_scenario_runs

   subroutine test_clock_forms(error)
      !! `HH:MM` and `HH:MM:SS` both parse, and mean what they say.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 1", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 M 100 at 06:30:15 runway 1", &
                                      "run until 09:00"])

      call run_scenario(sim, SCENARIO_PATH, 0_int64, .false., .false., err)
      call check(error,.not. err%has_error(), "HH:MM:SS was rejected")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) == &
                 6_tick_k*HOUR + 30_tick_k*MINUTE + 15000_tick_k, "seconds were dropped")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_clock_forms

   subroutine test_bad_clock(error)
      !! A time with no colon is a parse error.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 1", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 M 100 at 0630 runway 1"])

      call run_scenario(sim, SCENARIO_PATH, 0_int64, .false., .false., err)
      call check(error, err%has_error(), "'0630' was accepted as a time")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bad_clock

   subroutine test_bad_runway(error)
      !! An arrival onto a runway the airport does not have is rejected.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 1", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 M 100 at 06:00 runway 7"])

      call run_scenario(sim, SCENARIO_PATH, 0_int64, .false., .false., err)
      call check(error, err%has_error(), "runway 7 was accepted at a one-runway airport")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bad_runway

   subroutine test_no_stand(error)
      !! An aircraft no stand can ever take is a scenario error, not a wait.
      !!
      !! The tiny airport's only stand is Heavy. A Super would otherwise sit on
      !! the runway exit asking for a gate every minute until the horizon,
      !! which looks like gameplay and is actually a typo.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 1", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 S 500 at 06:00 runway 1"])

      call run_scenario(sim, SCENARIO_PATH, 0_int64, .false., .false., err)
      call check(error, err%has_error(), "a Super was accepted at a Heavy-only airport")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "stand") > 0, "the message does not mention stands")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_no_stand

   subroutine test_seed_override(error)
      !! `--seed` on the command line wins over `seed` in the script.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: scripted, overridden
      type(error_t) :: err

      call write_good_airport()
      call write_file(SCENARIO_PATH, [character(len=80) :: &
                                      "seed 42", &
                                      "load airport "//AIRPORT_PATH, &
                                      "arrival TEST01 M 100 at 06:00 runway 1", &
                                      "run until 09:00"])

      call run_scenario(scripted, SCENARIO_PATH, 0_int64, .false., .false., err)
      call run_scenario(overridden, SCENARIO_PATH, 4242_int64, .true., .false., err)
      call check(error,.not. err%has_error(), "a run reported an error")
      if (allocated(error)) return

      call check(error, scripted%seed == 42_int64, "the script's seed was not used")
      if (allocated(error)) return
      call check(error, overridden%seed == 4242_int64, "the command line seed was ignored")
      if (allocated(error)) return
      call check(error, scripted%log%digest() /= overridden%log%digest(), &
                 "the overriding seed changed nothing")

      call discard(SCENARIO_PATH)
      call discard(AIRPORT_PATH)
      call scripted%destroy()
      call overridden%destroy()
   end subroutine test_seed_override

end module test_app_loader
