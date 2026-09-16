! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The event record and its total ordering.
module core_event
   !! An event is six integers. It carries no pointer, no allocatable and no
   !! real, so it is trivially copied, trivially hashed and trivially written
   !! to a checkpoint.
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   implicit none
   private

   public :: event_t
   public :: event_less

   type :: event_t
      !! One scheduled occurrence.
      integer(tick_k) :: tick = 0_tick_k
         !! Sim time the event fires at.
      integer(int64) :: seq = 0_int64
         !! Monotonic scheduling sequence. This field is not optional: two
         !! aircraft reaching a hold point on the same tick is routine, and
         !! without a tie-break the queue's internal ordering -- an
         !! implementation detail -- would leak into gameplay.
      integer(int16) :: kind = 0_int16
         !! One of the `K_*` identifiers in `core_event_kinds`.
      integer(id_k) :: entity = NO_ID
         !! Usually an aircraft handle; interpretation depends on `kind`.
      integer(int32) :: generation = 0_int32
         !! Tombstone stamp taken when the event was scheduled. Zero means the
         !! event is not entity-scoped and can never go stale.
      integer(int64) :: payload = 0_int64
         !! Kind-dependent scalar.
   end type event_t

contains

   pure function event_less(left, right) result(is_less)
      !! Strict ordering: earlier tick first, then lower sequence.
      !!
      !! Total by construction, because `seq` is unique. A genuine min-heap
      !! predicate that says what it means -- the C++ design has to invert this
      !! comparison to fit `std::priority_queue`'s max-heap, which is a
      !! perennial source of confusion.
      type(event_t), intent(in) :: left
      type(event_t), intent(in) :: right
      logical :: is_less

      if (left%tick /= right%tick) then
         is_less = left%tick < right%tick
      else
         is_less = left%seq < right%seq
      end if
   end function event_less

end module core_event
