! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the TOML schedule loader.
module test_app_schedule
   !! A bank is generated traffic, which makes it the one input where "it
   !! loaded without complaining" is not evidence of anything. These tests
   !! check the counts, the ordering, and the property the whole approach rests
   !! on: that the same seed expands to the same timetable.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use app_airport, only: load_airport
   use app_schedule, only: load_schedule, parse_clock_text, wake_from_letter
   use core_sim, only: sim_t, tick_k, id_k, HOUR, MINUTE, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   use pic_error, only: error_t
   use pic_types, only: default_int, int32, int64
   implicit none
   private

   public :: collect_app_schedule_tests

   integer, parameter :: N_STANDS = 12
      !! Stands in the fixture airport. More than any test lands, so that no
      !! arrival is ever refused a clearance for want of somewhere to park.

   character(len=*), parameter :: AIRPORT_PATH = "test_schedule_airport.txt"
   character(len=*), parameter :: SCHEDULE_PATH = "test_schedule.toml"

contains

   subroutine collect_app_schedule_tests(testsuite)
      !! Register the schedule tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("clock_text_parses_both_forms", test_clock_forms), &
                  new_unittest("clock_text_rejects_rubbish", test_clock_rubbish), &
                  new_unittest("wake_letters_map", test_wake_letters), &
                  new_unittest("pinned_arrivals_keep_their_times", test_pinned), &
                  new_unittest("pinned_arrivals_come_first", test_pinned_first), &
                  new_unittest("bank_expands_to_its_count", test_bank_count), &
                  new_unittest("bank_times_are_sorted_and_in_range", test_bank_window), &
                  new_unittest("bank_is_reproducible", test_bank_reproducible), &
                  new_unittest("bank_follows_the_seed", test_bank_follows_seed), &
                  new_unittest("bank_honours_the_wake_mix", test_wake_mix), &
                  new_unittest("departure_bank_is_refused", test_departure_refused), &
                  new_unittest("schedule_needs_an_airport", test_needs_airport), &
                  new_unittest("malformed_toml_is_reported", test_malformed) &
                  ]
   end subroutine collect_app_schedule_tests

   pure function quoted(text) result(literal)
      !! `text` wrapped in double quotes, for building TOML fixtures.
      !!
      !! `achar(34)` rather than a doubled-quote literal. TOML wants double
      !! quotes around its strings, and a Fortran string expressing them by
      !! doubling is unreadable at exactly the moment you want to check a
      !! fixture by eye.
      character(len=*), intent(in) :: text
         !! Text to wrap.
      character(len=:), allocatable :: literal

      literal = achar(34)//text//achar(34)
   end function quoted

   subroutine write_lines(path, lines)
      !! Write a scratch file, one array element per line.
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
   end subroutine write_lines

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

   subroutine with_airport(sim, seed, err)
      !! A seeded simulation with an airport that has room for everybody.
      !!
      !! `init` is part of the fixture on purpose. Without it the bus has no
      !! subscriptions, so nothing dispatches, every `touchdown_tick` stays
      !! zero, and a test comparing them passes or fails for reasons that have
      !! nothing to do with the schedule.
      !!
      !! Twelve stands, which is more than any fixture here lands. That is
      !! deliberate: since the apron buffer arrived, an arrival with nowhere to
      !! park is refused its clearance and holds, so a cramped airport would
      !! push `touchdown_tick` outside the bank's window and these tests would
      !! start measuring contention instead of the loader.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      type(error_t), intent(inout) :: err

      ! Two runway nodes, one stand node each, one runway edge, one edge and
      ! one gate directive per stand, and the runway. Sized exactly, because
      ! one short is a segfault rather than a compile error.
      character(len=64) :: lines(4 + 3*N_STANDS)
      integer :: i, at

      call sim%init(seed, err)

      lines(1) = "node runway_threshold 0 0 S"
      lines(2) = "node runway_exit 1000 0 S"
      at = 2
      do i = 1, N_STANDS
         at = at + 1
         write (lines(at), "(a,i0,a)") "node gate ", 2000 + 100*i, " 0 S"
      end do
      at = at + 1
      lines(at) = "edge 1 2 30000"
      do i = 1, N_STANDS
         at = at + 1
         write (lines(at), "(a,i0,a)") "edge 2 ", 2 + i, " 40000"
      end do
      ! Stands and the runway come after every node the file refers to.
      do i = 1, N_STANDS
         at = at + 1
         write (lines(at), "(a,i0,a,i0,a)") "gate S", i, " ", 2 + i, " S"
      end do
      at = at + 1
      lines(at) = "runway 09 1 2"

      call write_lines(AIRPORT_PATH, lines(1:at))
      call load_airport(sim, AIRPORT_PATH, 256_default_int, err)
   end subroutine with_airport

   subroutine test_clock_forms(error)
      !! `HH:MM` and `HH:MM:SS`.
      type(error_type), allocatable, intent(out) :: error

      type(error_t) :: err
      integer(tick_k) :: tick

      call parse_clock_text("06:45", tick, err)
      call check(error, tick == 6_tick_k*HOUR + 45_tick_k*MINUTE, "HH:MM")
      if (allocated(error)) return

      call parse_clock_text("06:45:30", tick, err)
      call check(error, tick == 6_tick_k*HOUR + 45_tick_k*MINUTE + 30000_tick_k, "HH:MM:SS")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), "a valid clock reported an error")
   end subroutine test_clock_forms

   subroutine test_clock_rubbish(error)
      !! Strict parsing: a near-miss is an error, not a number.
      type(error_type), allocatable, intent(out) :: error

      type(error_t) :: err
      integer(tick_k) :: tick

      call parse_clock_text("0645", tick, err)
      call check(error, err%has_error(), "a time with no colon should be refused")
      if (allocated(error)) return

      block
         type(error_t) :: second
         call parse_clock_text("06:4x", tick, second)
         call check(error, second%has_error(), "a minute with a stray letter should be refused, not read as 6:4")
      end block
   end subroutine test_clock_rubbish

   subroutine test_wake_letters(error)
      !! Both cases map, and anything else is zero.
      type(error_type), allocatable, intent(out) :: error

      call check(error, wake_from_letter("H") == WAKE_HEAVY, "H")
      if (allocated(error)) return
      call check(error, wake_from_letter("s") == WAKE_SUPER, "lowercase s")
      if (allocated(error)) return
      call check(error, wake_from_letter("X") == 0_int32, "an unknown letter is zero")
   end subroutine test_wake_letters

   subroutine test_pinned(error)
      !! A pinned arrival lands at exactly the time it names.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call with_airport(sim, 42_int64, err)
      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[arrival]]", &
                                       "callsign = "//quoted("TEST01"), &
                                       "wake = "//quoted("H"), &
                                       "pax = 240", &
                                       "at = "//quoted("06:45"), &
                                       "runway = 1"])
      call load_schedule(sim, SCHEDULE_PATH, err)
      call check(error,.not. err%has_error(), "a valid schedule was rejected")
      if (allocated(error)) return

      call check(error, sim%world%aircraft%size() == 1_default_int, "one aircraft")
      if (allocated(error)) return
      call check(error, trim(sim%world%callsign(1)) == "TEST01", "callsign")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%wake(1) == WAKE_HEAVY, "wake")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%pax(1) == 240_int32, "pax")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_pinned

   subroutine test_pinned_first(error)
      !! Pinned movements take the low handles, whatever a bank does.
      !!
      !! A scenario that commands "aircraft 1" must keep meaning the same
      !! aircraft when somebody resizes a bank in the same file.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call with_airport(sim, 42_int64, err)
      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[bank]]", &
                                       "kind = "//quoted("arrival"), &
                                       "prefix = "//quoted("BNK"), &
                                       "from = "//quoted("08:00"), &
                                       "to = "//quoted("09:00"), &
                                       "count = 5", &
                                       "wake_mix = [0, 1, 0, 0]", &
                                       "", &
                                       "[[arrival]]", &
                                       "callsign = "//quoted("PINNED"), &
                                       "at = "//quoted("06:45")])
      call load_schedule(sim, SCHEDULE_PATH, err)
      call check(error,.not. err%has_error(), "schedule rejected")
      if (allocated(error)) return

      ! The bank is written first in the file and still loads second.
      call check(error, trim(sim%world%callsign(1)) == "PINNED", &
                 "the pinned arrival did not take handle 1")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%size() == 6_default_int, "one pinned plus five banked")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_pinned_first

   subroutine write_bank(count)
      !! A one-bank schedule of `count` medium arrivals in a fixed window.
      integer(int32), intent(in) :: count
         !! Movements to ask for, 1 to 9.

      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[bank]]", &
                                       "kind = "//quoted("arrival"), &
                                       "prefix = "//quoted("BNK"), &
                                       "from = "//quoted("08:00"), &
                                       "to = "//quoted("10:00"), &
                                       "count = "//achar(iachar("0") + int(count)), &
                                       "wake_mix = [0, 1, 0, 0]"])
   end subroutine write_bank

   subroutine test_bank_count(error)
      !! A bank creates exactly as many movements as it asks for.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call with_airport(sim, 42_int64, err)
      call write_bank(7_int32)
      call load_schedule(sim, SCHEDULE_PATH, err)

      call check(error,.not. err%has_error(), "bank rejected")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%size() == 7_default_int, "wrong number of movements")
      if (allocated(error)) return
      call check(error, trim(sim%world%callsign(1)) == "BNK001", "callsigns are prefixed and padded")
      if (allocated(error)) return
      call check(error, trim(sim%world%callsign(7)) == "BNK007", "last callsign")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bank_count

   subroutine test_bank_window(error)
      !! Times land inside the window and come out ascending.
      !!
      !! Ascending matters: a bank is a timetable, not the order the draws
      !! happened to come out in, and a scenario reading the board expects
      !! movement 2 to follow movement 1.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(default_int) :: i
      integer(tick_k) :: from_tick, to_tick

      from_tick = 8_tick_k*HOUR
      to_tick = 10_tick_k*HOUR

      call with_airport(sim, 42_int64, err)
      call write_bank(9_int32)
      call load_schedule(sim, SCHEDULE_PATH, err)
      call check(error,.not. err%has_error(), "bank rejected")
      if (allocated(error)) return

      ! Just past the window. Touchdown does not depend on a stand being
      ! free, so every movement has landed by here -- and a bounded horizon
      ! stops the run rather than following gate retries for ever.
      call sim%run_until(11_tick_k*HOUR, err)

      do i = 1_default_int, sim%world%aircraft%size()
         call check(error, sim%world%aircraft%touchdown_tick(i) >= from_tick .and. &
                    sim%world%aircraft%touchdown_tick(i) <= to_tick, &
                    "a movement landed outside the bank's window")
         if (allocated(error)) return
         if (i == 1_default_int) cycle
         call check(error, sim%world%aircraft%touchdown_tick(i) >= &
                    sim%world%aircraft%touchdown_tick(i - 1), &
                    "bank movements are not in time order")
         if (allocated(error)) return
      end do

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_bank_window

   subroutine test_bank_reproducible(error)
      !! The same seed expands to the same timetable.
      !!
      !! This is the whole justification for generating traffic rather than
      !! writing it out. If it did not hold, a bank would make every scenario
      !! that used one unreproducible.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: first, second
      type(error_t) :: err
      integer(default_int) :: i

      call with_airport(first, 42_int64, err)
      call write_bank(9_int32)
      call load_schedule(first, SCHEDULE_PATH, err)

      call with_airport(second, 42_int64, err)
      call load_schedule(second, SCHEDULE_PATH, err)
      call check(error,.not. err%has_error(), "a load reported an error")
      if (allocated(error)) return

      ! Both runs have to actually happen. Comparing `touchdown_tick` before
      ! the events fire compares two sets of zeros, which any loader passes.
      call first%run_until(11_tick_k*HOUR, err)
      call second%run_until(11_tick_k*HOUR, err)

      call check(error, first%world%aircraft%size() == second%world%aircraft%size(), "counts differ")
      if (allocated(error)) return

      ! Guard against the vacuous pass. Comparing `touchdown_tick` before the
      ! events fire compares two sets of zeros, and every loader passes that.
      call check(error, first%world%aircraft%touchdown_tick(1) > 0_tick_k, &
                 "nothing was dispatched, so this comparison proves nothing")
      if (allocated(error)) return

      do i = 1_default_int, first%world%aircraft%size()
         call check(error, first%world%aircraft%touchdown_tick(i) == &
                    second%world%aircraft%touchdown_tick(i), "a movement time moved")
         if (allocated(error)) return
         call check(error, first%world%aircraft%wake(i) == second%world%aircraft%wake(i), &
                    "a wake category moved")
         if (allocated(error)) return
      end do

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call first%destroy()
      call second%destroy()
   end subroutine test_bank_reproducible

   subroutine test_bank_follows_seed(error)
      !! A different seed gives a different timetable.
      !!
      !! Without this, `bank_is_reproducible` would pass on a loader that
      !! ignored the seed entirely and spread movements evenly.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: first, second
      type(error_t) :: err
      logical :: any_different
      integer(default_int) :: i

      call with_airport(first, 42_int64, err)
      call write_bank(9_int32)
      call load_schedule(first, SCHEDULE_PATH, err)

      call with_airport(second, 4242_int64, err)
      call load_schedule(second, SCHEDULE_PATH, err)

      call first%run_until(11_tick_k*HOUR, err)
      call second%run_until(11_tick_k*HOUR, err)

      ! Guard against the vacuous pass. Comparing `touchdown_tick` before the
      ! events fire compares two sets of zeros, and every loader passes that.
      call check(error, first%world%aircraft%touchdown_tick(1) > 0_tick_k, &
                 "nothing was dispatched, so this comparison proves nothing")
      if (allocated(error)) return

      any_different = .false.
      do i = 1_default_int, first%world%aircraft%size()
         if (first%world%aircraft%touchdown_tick(i) /= second%world%aircraft%touchdown_tick(i)) then
            any_different = .true.
         end if
      end do
      call check(error, any_different, "the seed made no difference to the bank")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call first%destroy()
      call second%destroy()
   end subroutine test_bank_follows_seed

   subroutine test_wake_mix(error)
      !! A mix naming one category produces only that category.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(default_int) :: i

      call with_airport(sim, 42_int64, err)
      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[bank]]", &
                                       "kind = "//quoted("arrival"), &
                                       "from = "//quoted("08:00"), &
                                       "to = "//quoted("09:00"), &
                                       "count = 8", &
                                       "wake_mix = [0, 0, 0, 1]"])
      call load_schedule(sim, SCHEDULE_PATH, err)
      call check(error,.not. err%has_error(), "bank rejected")
      if (allocated(error)) return

      do i = 1_default_int, sim%world%aircraft%size()
         call check(error, sim%world%aircraft%wake(i) == WAKE_SUPER, &
                    "a mix of only super produced something else")
         if (allocated(error)) return
      end do

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_wake_mix

   subroutine test_departure_refused(error)
      !! A departure bank fails loudly rather than loading nothing.
      !!
      !! A schedule written ahead of the departure manager would otherwise run
      !! light and look like a tuning problem.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err, fault

      call with_airport(sim, 42_int64, err)
      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[bank]]", &
                                       "kind = "//quoted("departure"), &
                                       "from = "//quoted("08:00"), &
                                       "to = "//quoted("09:00"), &
                                       "count = 4"])
      call load_schedule(sim, SCHEDULE_PATH, fault)
      call check(error, fault%has_error(), "a departure bank was silently ignored")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%size() == 0_default_int, "it created movements anyway")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_departure_refused

   subroutine test_needs_airport(error)
      !! A schedule without an airport is refused.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: fault

      call write_bank(3_int32)
      call load_schedule(sim, SCHEDULE_PATH, fault)
      call check(error, fault%has_error(), "a schedule loaded without an airport")

      call discard(SCHEDULE_PATH)
      call sim%destroy()
   end subroutine test_needs_airport

   subroutine test_malformed(error)
      !! Broken TOML comes back as a parse error, not a crash.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err, fault

      call with_airport(sim, 42_int64, err)
      call write_lines(SCHEDULE_PATH, [character(len=64) :: &
                                       "[[bank]", &
                                       "kind = "//quoted("arrival")])
      call load_schedule(sim, SCHEDULE_PATH, fault)
      call check(error, fault%has_error(), "malformed TOML was accepted")

      call discard(SCHEDULE_PATH)
      call discard(AIRPORT_PATH)
      call sim%destroy()
   end subroutine test_malformed

end module test_app_schedule
