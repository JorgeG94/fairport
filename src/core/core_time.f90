! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Integer sim time.
module core_time
   !! Sim time is an `integer(tick_k)` count of milliseconds and nothing else.
   !!
   !! There is no floating-point `dt` anywhere in fairport, and this module is
   !! why there does not need to be. Accumulating a real `dt` drifts, drift
   !! reorders simultaneous events, and event ordering is the simulation.
   use core_kinds, only: tick_k, int32
   implicit none
   private

   public :: MILLISECOND, SECOND, MINUTE, HOUR, DAY
   public :: tick_split

   integer(tick_k), parameter :: MILLISECOND = 1_tick_k
   integer(tick_k), parameter :: SECOND = 1000_tick_k
   integer(tick_k), parameter :: MINUTE = 60_tick_k*SECOND
   integer(tick_k), parameter :: HOUR = 60_tick_k*MINUTE
   integer(tick_k), parameter :: DAY = 24_tick_k*HOUR

contains

   pure subroutine tick_split(tick, hours, minutes, seconds, millis)
      !! Decompose a tick into wall-clock-looking components.
      !!
      !! Integer division throughout, and `modulo` rather than `mod` so that a
      !! negative tick still yields components in range. Formatting lives in
      !! `app/`; the core only ever does arithmetic.
      integer(tick_k), intent(in) :: tick
         !! Sim time to decompose.
      integer(int32), intent(out) :: hours
         !! Whole hours since midnight of the scenario day.
      integer(int32), intent(out) :: minutes
         !! Minutes within the hour, 0 to 59.
      integer(int32), intent(out) :: seconds
         !! Seconds within the minute, 0 to 59.
      integer(int32), intent(out) :: millis
         !! Milliseconds within the second, 0 to 999.

      integer(tick_k) :: rest

      rest = modulo(tick, DAY)
      hours = int(rest/HOUR, int32)
      rest = modulo(rest, HOUR)
      minutes = int(rest/MINUTE, int32)
      rest = modulo(rest, MINUTE)
      seconds = int(rest/SECOND, int32)
      millis = int(modulo(rest, SECOND), int32)
   end subroutine tick_split

end module core_time
