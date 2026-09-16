! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Text formatting for the terminal, without internal I/O.
module app_text
   !! Every number that reaches a golden file or a log line is rendered here.
   !!
   !! Not one of these functions uses a `write` statement. List-directed output
   !! is processor-dependent in field width, spacing and exponent digits, and
   !! even a formatted internal write is a place where two compilers can
   !! disagree about padding. A golden test that fails on Windows with what
   !! looks like whitespace noise and is actually a compiler difference costs a
   !! day to diagnose, so the rendering is arithmetic and `achar` all the way
   !! down.
   use core_sim, only: tick_k, tick_split
   use pic_types, only: default_int, int32, int64
   implicit none
   private

   public :: int_text
   public :: pad_left, pad_right
   public :: clock_text, duration_text

contains

   pure function int_text(value) result(text)
      !! Minimal-width decimal text.
      integer(int64), intent(in) :: value
         !! Value to render.
      character(len=:), allocatable :: text

      character(len=20) :: buffer
      integer(int64) :: rest
      integer(default_int) :: position

      if (value == 0_int64) then
         text = "0"
         return
      end if

      rest = abs(value)
      position = int(len(buffer), default_int)
      do while (rest > 0_int64)
         buffer(position:position) = achar(iachar("0") + int(mod(rest, 10_int64)))
         rest = rest/10_int64
         position = position - 1_default_int
      end do

      text = buffer(position + 1:)
      if (value < 0_int64) text = "-"//text
   end function int_text

   pure function pad_left(text, width) result(padded)
      !! Right-align `text` in a field of `width`, never truncating.
      character(len=*), intent(in) :: text
         !! Text to align.
      integer(default_int), intent(in) :: width
         !! Field width.
      character(len=:), allocatable :: padded

      if (len(text) >= width) then
         padded = text
      else
         padded = repeat(" ", int(width, default_int) - len(text))//text
      end if
   end function pad_left

   pure function pad_right(text, width) result(padded)
      !! Left-align `text` in a field of `width`, never truncating.
      character(len=*), intent(in) :: text
         !! Text to align.
      integer(default_int), intent(in) :: width
         !! Field width.
      character(len=:), allocatable :: padded

      if (len(text) >= width) then
         padded = text
      else
         padded = text//repeat(" ", int(width, default_int) - len(text))
      end if
   end function pad_right

   pure function clock_text(tick) result(text)
      !! Sim time as `HH:MM:SS`.
      integer(tick_k), intent(in) :: tick
         !! Sim time to render.
      character(len=8) :: text

      integer(int32) :: hours, minutes, seconds, millis

      call tick_split(tick, hours, minutes, seconds, millis)
      text = two_digits(hours)//":"//two_digits(minutes)//":"//two_digits(seconds)
   end function clock_text

   pure function duration_text(span) result(text)
      !! A span of sim time as minutes and seconds, `MMmSSs`.
      integer(tick_k), intent(in) :: span
         !! Span in milliseconds; negative spans render as zero.
      character(len=:), allocatable :: text

      integer(int64) :: total_seconds

      total_seconds = max(0_int64, span)/1000_int64
      text = int_text(total_seconds/60_int64)//"m"// &
             two_digits(int(mod(total_seconds, 60_int64), int32))//"s"
   end function duration_text

   pure function two_digits(value) result(text)
      !! A value in 0 to 99 as exactly two characters, zero filled.
      integer(int32), intent(in) :: value
         !! Value to render; anything outside 0 to 99 renders as `**`.
      character(len=2) :: text

      if (value < 0_int32 .or. value > 99_int32) then
         text = "**"
         return
      end if

      text(1:1) = achar(iachar("0") + int(value/10_int32))
      text(2:2) = achar(iachar("0") + int(mod(value, 10_int32)))
   end function two_digits

end module app_text
