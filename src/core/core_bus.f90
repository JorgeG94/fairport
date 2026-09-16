! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The event bus: who gets told that something happened.
module core_bus
   !! The scheduler is the temporal spine; the bus is structural decoupling.
   !! Both are needed and they are not the same mechanism.
   !!
   !! The C++ design reaches for `unordered_map<kind, handlers>` here and then
   !! has to add a rule forbidding iteration over it, because hash map order
   !! differs between standard library implementations. Event kinds are small
   !! dense integers, so this is a plain array indexed by kind, sized by the
   !! `MAX_EVENT_KIND` that fypp computes from the event list. No hashing, no
   !! allocation, and dispatch order that is deterministic by construction
   !! rather than by convention.
   use core_kinds, only: int16, int32
   use core_event, only: event_t
   use core_event_kinds, only: MAX_EVENT_KIND
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t
   use pic_types, only: default_int
   use pic_error, only: error_t, error_raise, ERROR_VALIDATION
   implicit none
   private

   public :: bus_t
   public :: MAX_HANDLERS_PER_KIND

   integer, parameter :: MAX_HANDLERS_PER_KIND = 8
      !! Systems that may subscribe to one kind. A fixed bound rather than a
      !! growable list: exceeding it is a design error worth an immediate
      !! complaint, not a silent reallocation.

   type :: subscriber_t
      !! One system's subscription to one event kind.
      private
      class(system_t), pointer :: sys => null()
         !! The subscribing system. A pointer because the sim owns the
         !! concrete instances and the bus only refers to them.
      integer(int32) :: order = 0_int32
         !! The system's declared `tick_order`, copied at subscribe time.
   end type subscriber_t

   type :: bus_t
      !! Handler table, indexed by event kind.
      private
      type(subscriber_t) :: subs(MAX_HANDLERS_PER_KIND, 0:MAX_EVENT_KIND)
         !! Subscribers per kind, kept sorted by `order`.
      integer(int32) :: n_subs(0:MAX_EVENT_KIND) = 0_int32
         !! How many slots of each column are in use.
   contains
      procedure :: subscribe => bus_subscribe
      procedure :: dispatch => bus_dispatch
      procedure :: subscriber_count => bus_subscriber_count
      procedure :: clear => bus_clear
   end type bus_t

contains

   subroutine bus_subscribe(this, kind, sys, err)
      !! Register `sys` as a handler of `kind`.
      !!
      !! Insertion sort on the system's declared order, with insertion index as
      !! the tie-break. Two systems that declare the same order therefore run
      !! in registration order, which the sim fixes in one place, so the total
      !! order never depends on module initialisation.
      class(bus_t), intent(inout) :: this
      integer(int16), intent(in) :: kind
         !! Event kind to subscribe to.
      class(system_t), target, intent(in) :: sys
         !! The subscribing system; must outlive the bus.
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: slot, column
      integer(int32) :: order

      column = int(kind, default_int)
      if (column < 0_default_int .or. column > int(MAX_EVENT_KIND, default_int)) then
         call error_raise(err, ERROR_VALIDATION, "bus_subscribe: event kind outside the generated range")
         return
      end if
      if (this%n_subs(column) >= MAX_HANDLERS_PER_KIND) then
         call error_raise(err, ERROR_VALIDATION, "bus_subscribe: too many handlers for one event kind")
         return
      end if

      order = sys%tick_order()

      slot = int(this%n_subs(column), default_int)
      do while (slot >= 1_default_int)
         if (this%subs(slot, column)%order <= order) exit
         this%subs(slot + 1, column) = this%subs(slot, column)
         slot = slot - 1_default_int
      end do

      this%subs(slot + 1, column)%sys => sys
      this%subs(slot + 1, column)%order = order
      this%n_subs(column) = this%n_subs(column) + 1_int32
   end subroutine bus_subscribe

   subroutine bus_dispatch(this, w, sched, event)
      !! Hand one event to every system subscribed to its kind, in order.
      class(bus_t), intent(inout) :: this
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event
         !! The event to deliver.

      integer(default_int) :: slot, column

      column = int(event%kind, default_int)
      if (column < 0_default_int .or. column > int(MAX_EVENT_KIND, default_int)) return

      do slot = 1_default_int, int(this%n_subs(column), default_int)
         call this%subs(slot, column)%sys%handle(w, sched, event)
      end do
   end subroutine bus_dispatch

   pure function bus_subscriber_count(this, kind) result(n)
      !! How many systems handle `kind`.
      class(bus_t), intent(in) :: this
      integer(int16), intent(in) :: kind
         !! Event kind to query.
      integer(int32) :: n

      integer(default_int) :: column

      n = 0_int32
      column = int(kind, default_int)
      if (column < 0_default_int .or. column > int(MAX_EVENT_KIND, default_int)) return
      n = this%n_subs(column)
   end function bus_subscriber_count

   subroutine bus_clear(this)
      !! Forget every subscription.
      class(bus_t), intent(inout) :: this

      integer(default_int) :: column, slot

      do column = 0_default_int, int(MAX_EVENT_KIND, default_int)
         do slot = 1_default_int, MAX_HANDLERS_PER_KIND
            this%subs(slot, column)%sys => null()
            this%subs(slot, column)%order = 0_int32
         end do
         this%n_subs(column) = 0_int32
      end do
   end subroutine bus_clear

end module core_bus
