! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The system contract.
module core_system
   !! A system owns behaviour. The world owns bytes.
   !!
   !! Every system may read anything in the world and is the documented sole
   !! writer of specific fields. Anything it wants another system to act on it
   !! says by scheduling an event, never by reaching across and writing that
   !! system's fields: if handlers mutated each other's state during dispatch,
   !! execution order would depend on subscription order, which depends on
   !! whatever was last edited.
   !!
   !! This is the one place inheritance earns its keep -- a handful of
   !! long-lived objects with genuinely different behaviour, dispatched a few
   !! thousand times a second rather than a few million.
   use core_kinds, only: int32
   use core_event, only: event_t
   use core_scheduler, only: scheduler_t
   use core_world, only: world_t
   implicit none
   private

   public :: system_t

   type, abstract :: system_t
      !! Base class for every simulation system.
   contains
      procedure(handle_i), deferred :: handle
      procedure(order_i), deferred :: tick_order
      procedure(name_i), deferred :: name
   end type system_t

   abstract interface

      subroutine handle_i(self, w, sched, event)
         !! React to one dispatched event.
         !!
         !! Each system does its own `select case (event%kind)`. That is
         !! coarser than a function pointer per event, and in practice better:
         !! one place per system where everything it reacts to is visible.
         import :: system_t, world_t, scheduler_t, event_t
         implicit none
         class(system_t), intent(inout) :: self
         type(world_t), intent(inout) :: w
         type(scheduler_t), intent(inout) :: sched
         type(event_t), intent(in) :: event
      end subroutine handle_i

      pure function order_i(self) result(order)
         !! Declared dispatch order. Lower runs first when two systems handle
         !! the same event. Declared, never inherited from registration order.
         import :: system_t, int32
         implicit none
         class(system_t), intent(in) :: self
         integer(int32) :: order
      end function order_i

      pure function name_i(self) result(name)
         !! Short system name, for logs.
         import :: system_t
         implicit none
         class(system_t), intent(in) :: self
         character(len=:), allocatable :: name
      end function name_i

   end interface

end module core_system
