! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for integer sim time.
module test_core_time
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_time, only: MILLISECOND, SECOND, MINUTE, HOUR, DAY, tick_split
   use core_kinds, only: tick_k, int32
   implicit none
   private

   public :: collect_core_time_tests

contains

   subroutine collect_core_time_tests(testsuite)
      !! Register the time tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("units_nest_correctly", test_units), &
                  new_unittest("split_decomposes_a_tick", test_split), &
                  new_unittest("split_wraps_at_a_day", test_split_wraps), &
                  new_unittest("split_handles_negative_ticks", test_split_negative), &
                  new_unittest("a_day_of_milliseconds_fits_easily", test_range) &
                  ]
   end subroutine collect_core_time_tests

   subroutine test_units(error)
      !! The constants are what their names say.
      type(error_type), allocatable, intent(out) :: error

      call check(error, MILLISECOND == 1_tick_k, "millisecond")
      if (allocated(error)) return
      call check(error, SECOND == 1000_tick_k, "second")
      if (allocated(error)) return
      call check(error, MINUTE == 60_tick_k*SECOND, "minute")
      if (allocated(error)) return
      call check(error, HOUR == 60_tick_k*MINUTE, "hour")
      if (allocated(error)) return
      call check(error, DAY == 24_tick_k*HOUR, "day")
      if (allocated(error)) return
      call check(error, DAY == 86400000_tick_k, "a day in milliseconds")
   end subroutine test_units

   subroutine test_split(error)
      !! An ordinary time comes apart into the pieces a clock shows.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: hours, minutes, seconds, millis

      call tick_split(6_tick_k*HOUR + 45_tick_k*MINUTE + 30_tick_k*SECOND + 123_tick_k, &
                      hours, minutes, seconds, millis)

      call check(error, hours == 6_int32, "hours")
      if (allocated(error)) return
      call check(error, minutes == 45_int32, "minutes")
      if (allocated(error)) return
      call check(error, seconds == 30_int32, "seconds")
      if (allocated(error)) return
      call check(error, millis == 123_int32, "milliseconds")
   end subroutine test_split

   subroutine test_split_wraps(error)
      !! Past midnight the hour restarts rather than reaching 24.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: hours, minutes, seconds, millis

      call tick_split(DAY, hours, minutes, seconds, millis)
      call check(error, hours == 0_int32, "exactly one day should be hour zero")
      if (allocated(error)) return

      call tick_split(DAY + 13_tick_k*HOUR + 5_tick_k*MINUTE, hours, minutes, seconds, millis)
      call check(error, hours == 13_int32, "hours after a day")
      if (allocated(error)) return
      call check(error, minutes == 5_int32, "minutes after a day")
   end subroutine test_split_wraps

   subroutine test_split_negative(error)
      !! A negative tick still yields components in range.
      !!
      !! `modulo`, not `mod`: `mod` takes the sign of the dividend, so a
      !! negative tick would give a negative hour. That is a real bug the
      !! moment anything subtracts two ticks and gets the order wrong.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: hours, minutes, seconds, millis

      call tick_split(-1_tick_k*HOUR, hours, minutes, seconds, millis)

      call check(error, hours == 23_int32, "an hour before the epoch should be 23:00")
      if (allocated(error)) return
      call check(error, minutes >= 0_int32 .and. minutes < 60_int32, "minutes out of range")
      if (allocated(error)) return
      call check(error, seconds >= 0_int32 .and. seconds < 60_int32, "seconds out of range")
      if (allocated(error)) return
      call check(error, millis >= 0_int32 .and. millis < 1000_int32, "milliseconds out of range")
   end subroutine test_split_negative

   subroutine test_range(error)
      !! Sixty-four bits of milliseconds is not a constraint anyone will meet.
      !!
      !! The 32-bit alternative wraps after about 25 days of sim time, which a
      !! long campaign reaches. This is the check that the kind is wide enough.
      type(error_type), allocatable, intent(out) :: error

      integer(tick_k) :: a_century

      a_century = 100_tick_k*365_tick_k*DAY
      call check(error, a_century > 0_tick_k, "a century of sim time overflowed")
      if (allocated(error)) return
      call check(error, a_century/DAY == 36500_tick_k, "a century did not round-trip")
   end subroutine test_range

end module test_core_time
