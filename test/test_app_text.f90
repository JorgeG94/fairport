! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the text formatter.
module test_app_text
   !! Every number that reaches a log line or a golden file is rendered by
   !! `app_text`, and none of it goes through a `write` statement.
   !!
   !! That is a deliberate choice with a cost: the arithmetic is hand-written,
   !! so it can be wrong in ways the compiler cannot see. List-directed output
   !! would have been shorter and would have differed between gfortran and ifx
   !! in field width and spacing, turning a golden-file failure into a
   !! whitespace mystery. These tests are what buys the choice back.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use app_text, only: int_text, pad_left, pad_right, clock_text, duration_text
   use core_sim, only: tick_k, SECOND, MINUTE, HOUR, DAY
   use pic_types, only: default_int, int64
   implicit none
   private

   public :: collect_app_text_tests

contains

   subroutine collect_app_text_tests(testsuite)
      !! Register the formatter tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("int_text_renders_decimals", test_int_text), &
                  new_unittest("int_text_handles_the_extremes", test_int_text_extremes), &
                  new_unittest("pad_left_right_aligns", test_pad_left), &
                  new_unittest("pad_right_left_aligns", test_pad_right), &
                  new_unittest("padding_never_truncates", test_pad_no_truncate), &
                  new_unittest("clock_text_is_fixed_width", test_clock_text), &
                  new_unittest("clock_text_wraps_at_midnight", test_clock_wraps), &
                  new_unittest("duration_text_reads_as_minutes", test_duration), &
                  new_unittest("duration_text_floors_negatives", test_duration_negative) &
                  ]
   end subroutine collect_app_text_tests

   subroutine test_int_text(error)
      !! Ordinary values render without padding or sign surprises.
      type(error_type), allocatable, intent(out) :: error

      call check(error, int_text(0_int64) == "0", "zero")
      if (allocated(error)) return
      call check(error, int_text(7_int64) == "7", "single digit")
      if (allocated(error)) return
      call check(error, int_text(42_int64) == "42", "two digits")
      if (allocated(error)) return
      call check(error, int_text(1000_int64) == "1000", "a trailing zero is not eaten")
      if (allocated(error)) return
      call check(error, int_text(1020304_int64) == "1020304", "interior zeros survive")
      if (allocated(error)) return
      call check(error, int_text(-5_int64) == "-5", "negative")
      if (allocated(error)) return
      call check(error, int_text(-1000_int64) == "-1000", "negative with trailing zeros")
   end subroutine test_int_text

   subroutine test_int_text_extremes(error)
      !! The 64-bit ends, where a naive `abs` overflows.
      type(error_type), allocatable, intent(out) :: error

      call check(error, int_text(huge(0_int64)) == "9223372036854775807", "huge")
      if (allocated(error)) return
      call check(error, len(int_text(huge(0_int64))) == 19, "huge is 19 digits")
      if (allocated(error)) return
      ! -huge - 1 has no positive counterpart; abs() of it is itself. The
      ! renderer must not silently produce nonsense for it.
      call check(error, len(int_text(-huge(0_int64))) == 20, "-huge is 20 characters")
      if (allocated(error)) return
      call check(error, int_text(-huge(0_int64)) == "-9223372036854775807", "-huge")
   end subroutine test_int_text_extremes

   subroutine test_pad_left(error)
      !! Right alignment for numeric columns.
      type(error_type), allocatable, intent(out) :: error

      call check(error, pad_left("7", 3_default_int) == "  7", "single character")
      if (allocated(error)) return
      call check(error, pad_left("42", 4_default_int) == "  42", "two characters")
      if (allocated(error)) return
      call check(error, len(pad_left("x", 6_default_int)) == 6, "width is honoured")
      if (allocated(error)) return
      call check(error, pad_left("", 2_default_int) == "  ", "empty input")
   end subroutine test_pad_left

   subroutine test_pad_right(error)
      !! Left alignment for text columns.
      type(error_type), allocatable, intent(out) :: error

      call check(error, pad_right("ab", 5_default_int) == "ab   ", "short text")
      if (allocated(error)) return
      call check(error, len(pad_right("ab", 5_default_int)) == 5, "width is honoured")
   end subroutine test_pad_right

   subroutine test_pad_no_truncate(error)
      !! Over-long text is returned whole, never cut.
      !!
      !! A truncated callsign on an ops board is worse than a ragged column:
      !! the column is obviously wrong, the callsign silently names a different
      !! aircraft.
      type(error_type), allocatable, intent(out) :: error

      call check(error, pad_left("abcdef", 3_default_int) == "abcdef", "pad_left truncated")
      if (allocated(error)) return
      call check(error, pad_right("abcdef", 3_default_int) == "abcdef", "pad_right truncated")
      if (allocated(error)) return
      call check(error, pad_left("abc", 3_default_int) == "abc", "exact fit")
   end subroutine test_pad_no_truncate

   subroutine test_clock_text(error)
      !! Always eight characters, always zero filled.
      type(error_type), allocatable, intent(out) :: error

      call check(error, clock_text(0_tick_k) == "00:00:00", "midnight")
      if (allocated(error)) return
      call check(error, clock_text(6_tick_k*HOUR + 45_tick_k*MINUTE) == "06:45:00", "a morning slot")
      if (allocated(error)) return
      call check(error, clock_text(23_tick_k*HOUR + 59_tick_k*MINUTE + 59_tick_k*SECOND) == "23:59:59", &
                 "one second to midnight")
      if (allocated(error)) return
      ! Sub-second remainders are dropped, not rounded up into the next second.
      call check(error, clock_text(6_tick_k*HOUR + 999_tick_k) == "06:00:00", "milliseconds are truncated")
      if (allocated(error)) return
      call check(error, len(clock_text(0_tick_k)) == 8, "width")
   end subroutine test_clock_text

   subroutine test_clock_wraps(error)
      !! Past midnight the clock wraps rather than running to 24 and beyond.
      type(error_type), allocatable, intent(out) :: error

      call check(error, clock_text(DAY) == "00:00:00", "exactly one day")
      if (allocated(error)) return
      call check(error, clock_text(DAY + 3_tick_k*HOUR) == "03:00:00", "a day and three hours")
      if (allocated(error)) return
      ! `modulo`, not `mod`: a negative tick must still land in range rather
      ! than rendering as a negative hour.
      call check(error, clock_text(-1_tick_k*HOUR) == "23:00:00", "an hour before the epoch")
   end subroutine test_clock_wraps

   subroutine test_duration(error)
      !! Spans read as minutes and seconds.
      type(error_type), allocatable, intent(out) :: error

      call check(error, duration_text(0_tick_k) == "0m00s", "zero")
      if (allocated(error)) return
      call check(error, duration_text(45_tick_k*SECOND) == "0m45s", "under a minute")
      if (allocated(error)) return
      call check(error, duration_text(MINUTE) == "1m00s", "exactly a minute")
      if (allocated(error)) return
      call check(error, duration_text(2_tick_k*MINUTE + 41_tick_k*SECOND) == "2m41s", "the taxi column")
      if (allocated(error)) return
      call check(error, duration_text(90_tick_k*MINUTE) == "90m00s", "minutes do not wrap into hours")
   end subroutine test_duration

   subroutine test_duration_negative(error)
      !! A negative span renders as zero, not as a negative minute count.
      type(error_type), allocatable, intent(out) :: error

      call check(error, duration_text(-1_tick_k*MINUTE) == "0m00s", "negative span")
   end subroutine test_duration_negative

end module test_app_text
