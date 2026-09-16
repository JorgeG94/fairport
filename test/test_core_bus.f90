! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the event bus and its dispatch order.
module test_core_bus
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_bus, only: bus_t, MAX_HANDLERS_PER_KIND
   use core_event, only: event_t
   use core_event_kinds, only: K_TOUCHDOWN, K_ONBLOCKS
   use core_kinds, only: id_k, int16, int32, NO_ID
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t
   use pic_error, only: error_t
   implicit none
   private

   public :: collect_core_bus_tests

   integer(int32), parameter :: MAX_TRACE = 16
      !! Handler calls one test will record.

   type :: trace_t
      !! Shared record of which systems ran, in which order.
      integer(int32) :: marks(MAX_TRACE) = 0_int32
      integer(int32) :: n = 0_int32
   end type trace_t

   type(trace_t), save :: trace
      !! Module state, which the style guide forbids in the library and which
      !! is the only practical way for a handler with a fixed interface to tell
      !! a test what it did. Confined to this test module, and reset before
      !! every test that reads it.

   type, extends(system_t) :: probe_system_t
      !! A system that records that it ran and nothing else.
      integer(int32) :: mark = 0_int32
         !! Value written to the trace when this system handles an event.
      integer(int32) :: order = 0_int32
         !! Declared dispatch order.
   contains
      procedure :: handle => probe_handle
      procedure :: tick_order => probe_tick_order
      procedure :: name => probe_name
   end type probe_system_t

contains

   subroutine collect_core_bus_tests(testsuite)
      !! Register the bus tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("dispatch_follows_tick_order", test_tick_order), &
                  new_unittest("registration_order_is_irrelevant", test_registration_order), &
                  new_unittest("equal_order_is_registration_order", test_equal_order), &
                  new_unittest("unsubscribed_kind_is_silent", test_unsubscribed), &
                  new_unittest("overflowing_a_kind_is_an_error", test_overflow) &
                  ]
   end subroutine collect_core_bus_tests

   subroutine probe_handle(self, w, sched, event)
      !! Record that this system saw the event.
      class(probe_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      if (trace%n >= MAX_TRACE) return
      trace%n = trace%n + 1_int32
      trace%marks(trace%n) = self%mark
   end subroutine probe_handle

   pure function probe_tick_order(self) result(order)
      !! Declared dispatch order.
      class(probe_system_t), intent(in) :: self
      integer(int32) :: order

      order = self%order
   end function probe_tick_order

   pure function probe_name(self) result(name)
      !! Short name.
      class(probe_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "probe"
   end function probe_name

   subroutine dispatch_one(bus, kind)
      !! Send one event of `kind` through `bus`.
      type(bus_t), intent(inout) :: bus
      integer(int16), intent(in) :: kind
         !! Event kind to dispatch.

      type(world_t) :: w
      type(scheduler_t) :: sched
      type(event_t) :: event

      event%kind = kind
      event%entity = NO_ID
      call bus%dispatch(w, sched, event)
   end subroutine dispatch_one

   subroutine test_tick_order(error)
      !! Lower declared order runs first.
      type(error_type), allocatable, intent(out) :: error

      type(bus_t) :: bus
      type(probe_system_t), target :: late, early
      type(error_t) :: err

      late%mark = 2_int32
      late%order = 20_int32
      early%mark = 1_int32
      early%order = 10_int32

      trace%n = 0_int32
      call bus%subscribe(K_TOUCHDOWN, late, err)
      call bus%subscribe(K_TOUCHDOWN, early, err)
      call dispatch_one(bus, K_TOUCHDOWN)

      call check(error, trace%n == 2_int32, "both systems should have run")
      if (allocated(error)) return
      call check(error, trace%marks(1) == 1_int32, "lower tick_order did not run first")
      if (allocated(error)) return
      call check(error, trace%marks(2) == 2_int32, "higher tick_order did not run second")
   end subroutine test_tick_order

   subroutine test_registration_order(error)
      !! Subscribing in the opposite order changes nothing.
      !!
      !! This is the property that keeps dispatch independent of module
      !! initialisation order, and so of whatever was last edited.
      type(error_type), allocatable, intent(out) :: error

      type(bus_t) :: forwards, backwards
      type(probe_system_t), target :: first, second
      type(error_t) :: err
      integer(int32) :: forward_marks(2)

      first%mark = 1_int32
      first%order = 10_int32
      second%mark = 2_int32
      second%order = 20_int32

      trace%n = 0_int32
      call forwards%subscribe(K_TOUCHDOWN, first, err)
      call forwards%subscribe(K_TOUCHDOWN, second, err)
      call dispatch_one(forwards, K_TOUCHDOWN)
      forward_marks = trace%marks(1:2)

      trace%n = 0_int32
      call backwards%subscribe(K_TOUCHDOWN, second, err)
      call backwards%subscribe(K_TOUCHDOWN, first, err)
      call dispatch_one(backwards, K_TOUCHDOWN)

      call check(error, all(trace%marks(1:2) == forward_marks), &
                 "registration order changed dispatch order")
   end subroutine test_registration_order

   subroutine test_equal_order(error)
      !! Two systems declaring the same order run in registration order.
      type(error_type), allocatable, intent(out) :: error

      type(bus_t) :: bus
      type(probe_system_t), target :: first, second
      type(error_t) :: err

      first%mark = 1_int32
      first%order = 5_int32
      second%mark = 2_int32
      second%order = 5_int32

      trace%n = 0_int32
      call bus%subscribe(K_TOUCHDOWN, first, err)
      call bus%subscribe(K_TOUCHDOWN, second, err)
      call dispatch_one(bus, K_TOUCHDOWN)

      call check(error, trace%n == 2_int32, "both systems should have run")
      if (allocated(error)) return
      call check(error, trace%marks(1) == 1_int32 .and. trace%marks(2) == 2_int32, &
                 "equal orders did not fall back to registration order")
   end subroutine test_equal_order

   subroutine test_unsubscribed(error)
      !! An event nobody handles is not an error.
      type(error_type), allocatable, intent(out) :: error

      type(bus_t) :: bus
      type(probe_system_t), target :: probe
      type(error_t) :: err

      probe%mark = 1_int32
      probe%order = 0_int32

      trace%n = 0_int32
      call bus%subscribe(K_TOUCHDOWN, probe, err)
      call dispatch_one(bus, K_ONBLOCKS)

      call check(error, trace%n == 0_int32, "a system ran for a kind it never subscribed to")
      if (allocated(error)) return
      call check(error, bus%subscriber_count(K_ONBLOCKS) == 0_int32, "unexpected subscriber count")
   end subroutine test_unsubscribed

   subroutine test_overflow(error)
      !! More handlers than the fixed bound is a loud error, not a silent drop.
      type(error_type), allocatable, intent(out) :: error

      type(bus_t) :: bus
      type(probe_system_t), target :: probe
      type(error_t) :: err
      integer(int32) :: i

      probe%mark = 1_int32
      probe%order = 0_int32

      do i = 1_int32, int(MAX_HANDLERS_PER_KIND, int32)
         call bus%subscribe(K_TOUCHDOWN, probe, err)
      end do
      call check(error,.not. err%has_error(), "filling the table should not be an error")
      if (allocated(error)) return

      call bus%subscribe(K_TOUCHDOWN, probe, err)
      call check(error, err%has_error(), "overflowing the table should be an error")
      if (allocated(error)) return
      call check(error, bus%subscriber_count(K_TOUCHDOWN) == int(MAX_HANDLERS_PER_KIND, int32), &
                 "subscriber count grew past the bound")
   end subroutine test_overflow

end module test_core_bus
