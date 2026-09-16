! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the event queue and its total ordering.
module test_core_scheduler
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_event, only: event_t, event_less
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_scheduler, only: scheduler_t
   use pic_error, only: error_t
   implicit none
   private

   public :: collect_core_scheduler_tests

   integer(int16), parameter :: KIND_A = 1_int16
   integer(int16), parameter :: KIND_B = 2_int16

contains

   subroutine collect_core_scheduler_tests(testsuite)
      !! Register the scheduler tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("pops_in_tick_order", test_tick_order), &
                  new_unittest("ties_break_fifo", test_ties_break_fifo), &
                  new_unittest("seq_is_monotonic", test_seq_monotonic), &
                  new_unittest("payload_survives", test_payload_survives), &
                  new_unittest("empty_pop_is_an_error", test_empty_pop), &
                  new_unittest("grows_past_initial_capacity", test_growth), &
                  new_unittest("event_less_is_total", test_event_less_total) &
                  ]
   end subroutine collect_core_scheduler_tests

   subroutine test_tick_order(error)
      !! Events come back earliest first, whatever order they went in.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event
      integer(tick_k) :: previous

      call sched%push(at=300_tick_k, kind=KIND_A, entity=1_id_k)
      call sched%push(at=100_tick_k, kind=KIND_A, entity=2_id_k)
      call sched%push(at=200_tick_k, kind=KIND_A, entity=3_id_k)

      previous = -1_tick_k
      do while (.not. sched%is_empty())
         call sched%pop(event)
         call check(error, event%tick >= previous, "events came back out of order")
         if (allocated(error)) return
         previous = event%tick
      end do

      call check(error, previous == 300_tick_k, "last event was not the latest")
      call sched%destroy()
   end subroutine test_tick_order

   subroutine test_ties_break_fifo(error)
      !! Simultaneous events come back in the order they were scheduled.
      !!
      !! Two aircraft reaching a hold point on the same tick is routine. This
      !! is the property that stops the queue's internals leaking into
      !! gameplay, and it is the whole reason `seq` exists.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event
      integer(id_k) :: expected

      call sched%push(at=500_tick_k, kind=KIND_A, entity=1_id_k)
      call sched%push(at=500_tick_k, kind=KIND_A, entity=2_id_k)
      call sched%push(at=500_tick_k, kind=KIND_A, entity=3_id_k)
      call sched%push(at=500_tick_k, kind=KIND_A, entity=4_id_k)

      expected = 1_id_k
      do while (.not. sched%is_empty())
         call sched%pop(event)
         call check(error, event%entity == expected, "simultaneous events are not FIFO")
         if (allocated(error)) return
         expected = expected + 1_id_k
      end do

      call sched%destroy()
   end subroutine test_ties_break_fifo

   subroutine test_seq_monotonic(error)
      !! Every event gets a distinct, increasing sequence number.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event
      integer(int64) :: seen(3)
      integer(int32) :: i

      call sched%push(at=300_tick_k, kind=KIND_A, entity=1_id_k)
      call sched%push(at=100_tick_k, kind=KIND_A, entity=2_id_k)
      call sched%push(at=200_tick_k, kind=KIND_A, entity=3_id_k)

      do i = 1_int32, 3_int32
         call sched%pop(event)
         seen(i) = event%seq
      end do

      ! Popped in tick order 100, 200, 300, which were pushed second, third
      ! and first, so the sequence numbers come back as 1, 2, 0.
      call check(error, seen(1) == 1_int64, "first popped has the wrong seq")
      if (allocated(error)) return
      call check(error, seen(2) == 2_int64, "second popped has the wrong seq")
      if (allocated(error)) return
      call check(error, seen(3) == 0_int64, "third popped has the wrong seq")

      call sched%destroy()
   end subroutine test_seq_monotonic

   subroutine test_payload_survives(error)
      !! Every field of an event comes back as it went in.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event

      call sched%push(at=1234_tick_k, kind=KIND_B, entity=7_id_k, &
                      generation=9_int32, payload=-42_int64)
      call sched%pop(event)

      call check(error, event%tick == 1234_tick_k, "tick")
      if (allocated(error)) return
      call check(error, event%kind == KIND_B, "kind")
      if (allocated(error)) return
      call check(error, event%entity == 7_id_k, "entity")
      if (allocated(error)) return
      call check(error, event%generation == 9_int32, "generation")
      if (allocated(error)) return
      call check(error, event%payload == -42_int64, "payload")

      call sched%destroy()
   end subroutine test_payload_survives

   subroutine test_empty_pop(error)
      !! Popping nothing is an error, not a crash.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event
      type(error_t) :: err

      call sched%pop(event, err)
      call check(error, err%has_error(), "an empty pop should report an error")
      if (allocated(error)) return
      call check(error, sched%is_empty(), "queue should still be empty")

      call sched%destroy()
   end subroutine test_empty_pop

   subroutine test_growth(error)
      !! The event pool grows past its initial capacity without losing events.
      type(error_type), allocatable, intent(out) :: error

      type(scheduler_t) :: sched
      type(event_t) :: event
      integer(int32) :: i
      integer(int32), parameter :: N = 1000_int32
      integer(tick_k) :: previous

      do i = 1_int32, N
         call sched%push(at=int(N - i, tick_k), kind=KIND_A, entity=int(i, id_k))
      end do

      call check(error, sched%pending() == int(N, kind(sched%pending())), "wrong number pending")
      if (allocated(error)) return

      previous = -1_tick_k
      do i = 1_int32, N
         call sched%pop(event)
         call check(error, event%tick >= previous, "ordering broke across a growth")
         if (allocated(error)) return
         previous = event%tick
      end do

      call check(error, sched%is_empty(), "queue should be drained")
      call sched%destroy()
   end subroutine test_growth

   subroutine test_event_less_total(error)
      !! The ordering predicate is strict and total.
      type(error_type), allocatable, intent(out) :: error

      type(event_t) :: early, late, same_tick

      early%tick = 10_tick_k
      early%seq = 0_int64
      late%tick = 20_tick_k
      late%seq = 0_int64
      same_tick%tick = 10_tick_k
      same_tick%seq = 1_int64

      call check(error, event_less(early, late), "earlier tick should come first")
      if (allocated(error)) return
      call check(error,.not. event_less(late, early), "ordering is not antisymmetric")
      if (allocated(error)) return
      call check(error, event_less(early, same_tick), "lower seq should come first on a tie")
      if (allocated(error)) return
      call check(error,.not. event_less(early, early), "an event should not precede itself")
   end subroutine test_event_less_total

end module test_core_scheduler
