! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for space-time reservations over the taxiway graph.
module test_core_reservations
   !! Two aircraft on one taxiway node at one instant is the thing this exists
   !! to prevent, and `no_two_aircraft_share_a_node` checks it against a real
   !! run rather than against the pool in isolation.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_reservations, only: reservation_pool_t
   use core_sim, only: sim_t, HOUR, MINUTE, SECOND, DEFAULT_FUEL_MS, &
                       WAKE_MEDIUM, WAKE_SUPER, PHASE_APPROACH
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_core_reservations_tests

   integer(int32), parameter :: N_NODES = 6_int32
      !! Nodes in the pool fixtures.
   integer(default_int), parameter :: POOL_CAP = 32_default_int
      !! Slots in the pool fixtures.

contains

   subroutine collect_core_reservations_tests(testsuite)
      !! Register the reservation tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("an_empty_pool_is_all_free", test_empty), &
                  new_unittest("a_claim_blocks_its_window", test_claim), &
                  new_unittest("windows_are_half_open", test_half_open), &
                  new_unittest("other_nodes_are_unaffected", test_other_nodes), &
                  new_unittest("an_aircraft_ignores_itself", test_ignores_self), &
                  new_unittest("release_gives_everything_back", test_release), &
                  new_unittest("slots_are_recycled", test_recycle), &
                  new_unittest("exhaustion_is_an_error", test_exhaustion), &
                  new_unittest("an_unsized_pool_constrains_nothing", test_unsized), &
                  new_unittest("no_two_aircraft_share_a_node", test_no_overlap_in_practice) &
                  ]
   end subroutine collect_core_reservations_tests

   subroutine fresh(pool, err)
      !! A sized, empty pool.
      type(reservation_pool_t), intent(inout) :: pool
      type(error_t), intent(inout) :: err

      call pool%reserve_pool(N_NODES, POOL_CAP, err)
   end subroutine fresh

   subroutine test_empty(error)
      !! Nothing is claimed to begin with.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call check(error,.not. err%has_error(), "sizing reported an error")
      if (allocated(error)) return

      call check(error, pool%is_free(1_id_k, 0_tick_k, 1000_tick_k, NO_ID), "node 1")
      if (allocated(error)) return
      call check(error, pool%in_use() == 0_default_int, "a fresh pool holds something")
      if (allocated(error)) return
      call check(error, pool%capacity() == POOL_CAP, "capacity")

      call pool%destroy()
   end subroutine test_empty

   subroutine test_claim(error)
      !! A claim blocks exactly its window.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call pool%claim(2_id_k, 1000_tick_k, 2000_tick_k, 7_id_k, err)
      call check(error,.not. err%has_error(), "claim reported an error")
      if (allocated(error)) return

      call check(error,.not. pool%is_free(2_id_k, 1500_tick_k, 1600_tick_k, NO_ID), &
                 "a window inside the claim should be blocked")
      if (allocated(error)) return
      call check(error,.not. pool%is_free(2_id_k, 500_tick_k, 1500_tick_k, NO_ID), &
                 "a window overlapping the start should be blocked")
      if (allocated(error)) return
      call check(error, pool%is_free(2_id_k, 2500_tick_k, 3000_tick_k, NO_ID), &
                 "a window after the claim should be free")
      if (allocated(error)) return
      call check(error, pool%held_by(2_id_k, 1500_tick_k) == 7_id_k, "held_by")
      if (allocated(error)) return
      call check(error, pool%in_use() == 1_default_int, "one reservation")

      call pool%destroy()
   end subroutine test_claim

   subroutine test_half_open(error)
      !! Touching windows do not conflict.
      !!
      !! An aircraft leaving a node at the instant the next one arrives is a
      !! handover, not a conflict. Treating it as one would deadlock an apron
      !! with a single entrance, because every aircraft would be waiting for a
      !! node the one ahead had just stopped needing.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call pool%claim(3_id_k, 1000_tick_k, 2000_tick_k, 1_id_k, err)

      call check(error, pool%is_free(3_id_k, 2000_tick_k, 3000_tick_k, NO_ID), &
                 "a window starting where the last ended should be free")
      if (allocated(error)) return
      call check(error, pool%is_free(3_id_k, 0_tick_k, 1000_tick_k, NO_ID), &
                 "a window ending where the next begins should be free")
      if (allocated(error)) return
      call check(error,.not. pool%is_free(3_id_k, 1999_tick_k, 2500_tick_k, NO_ID), &
                 "one millisecond of overlap is still a conflict")

      call pool%destroy()
   end subroutine test_half_open

   subroutine test_other_nodes(error)
      !! A claim on one node says nothing about another.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call pool%claim(2_id_k, 1000_tick_k, 2000_tick_k, 1_id_k, err)

      call check(error, pool%is_free(3_id_k, 1000_tick_k, 2000_tick_k, NO_ID), &
                 "claiming node 2 blocked node 3")
      if (allocated(error)) return
      call check(error, pool%is_free(1_id_k, 1000_tick_k, 2000_tick_k, NO_ID), &
                 "claiming node 2 blocked node 1")

      call pool%destroy()
   end subroutine test_other_nodes

   subroutine test_ignores_self(error)
      !! An aircraft does not conflict with its own reservations.
      !!
      !! A replan has to be checked against everybody else while the route it
      !! is replacing is still held, or an aircraft could never change its mind.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call pool%claim(2_id_k, 1000_tick_k, 2000_tick_k, 5_id_k, err)

      call check(error, pool%is_free(2_id_k, 1000_tick_k, 2000_tick_k, 5_id_k), &
                 "an aircraft conflicted with itself")
      if (allocated(error)) return
      call check(error,.not. pool%is_free(2_id_k, 1000_tick_k, 2000_tick_k, 6_id_k), &
                 "another aircraft was let through")

      call pool%destroy()
   end subroutine test_ignores_self

   subroutine test_release(error)
      !! Releasing gives back everything one aircraft held, and only that.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err

      call fresh(pool, err)
      call pool%claim(1_id_k, 0_tick_k, 1000_tick_k, 5_id_k, err)
      call pool%claim(2_id_k, 1000_tick_k, 2000_tick_k, 5_id_k, err)
      call pool%claim(3_id_k, 0_tick_k, 1000_tick_k, 6_id_k, err)
      call check(error, pool%in_use() == 3_default_int, "three reservations")
      if (allocated(error)) return

      call pool%release_all(5_id_k)

      call check(error, pool%in_use() == 1_default_int, "only the other aircraft should remain")
      if (allocated(error)) return
      call check(error, pool%is_free(1_id_k, 0_tick_k, 1000_tick_k, NO_ID), "node 1 not released")
      if (allocated(error)) return
      call check(error, pool%is_free(2_id_k, 1000_tick_k, 2000_tick_k, NO_ID), "node 2 not released")
      if (allocated(error)) return
      call check(error,.not. pool%is_free(3_id_k, 0_tick_k, 1000_tick_k, NO_ID), &
                 "somebody else's reservation was released too")

      call pool%destroy()
   end subroutine test_release

   subroutine test_recycle(error)
      !! Released slots come back, so a long day does not exhaust the pool.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err
      integer(int32) :: round

      call fresh(pool, err)

      ! Many more movements than the pool has slots.
      do round = 1_int32, 200_int32
         call pool%claim(1_id_k, 0_tick_k, 1000_tick_k, int(round, id_k), err)
         call pool%release_all(int(round, id_k))
      end do

      call check(error,.not. err%has_error(), "the pool ran out despite every slot being released")
      if (allocated(error)) return
      call check(error, pool%in_use() == 0_default_int, "slots leaked")

      call pool%destroy()
   end subroutine test_recycle

   subroutine test_exhaustion(error)
      !! Running out is a loud error, not a silent drop.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool
      type(error_t) :: err, fault
      integer(int32) :: i

      call pool%reserve_pool(N_NODES, 2_default_int, err)
      call pool%claim(1_id_k, 0_tick_k, 10_tick_k, 1_id_k, err)
      call pool%claim(1_id_k, 20_tick_k, 30_tick_k, 2_id_k, err)
      call check(error,.not. err%has_error(), "filling the pool should not error")
      if (allocated(error)) return

      call pool%claim(1_id_k, 40_tick_k, 50_tick_k, 3_id_k, fault)
      call check(error, fault%has_error(), "overflowing the pool was accepted")

      call pool%destroy()
   end subroutine test_exhaustion

   subroutine test_unsized(error)
      !! A pool that was never sized constrains nothing and does not crash.
      !!
      !! `size` of an unallocated array is undefined rather than zero, so this
      !! was a segfault in four test fixtures before the guard went in.
      type(error_type), allocatable, intent(out) :: error

      type(reservation_pool_t) :: pool

      call check(error, pool%is_free(1_id_k, 0_tick_k, 1000_tick_k, NO_ID), &
                 "an unsized pool should report everything free")
      if (allocated(error)) return
      call check(error, pool%held_by(1_id_k, 0_tick_k) == NO_ID, "an unsized pool holds nobody")
      if (allocated(error)) return
      call check(error, pool%in_use() == 0_default_int, "an unsized pool is empty")

      call pool%release_all(1_id_k)
   end subroutine test_unsized

   subroutine test_no_overlap_in_practice(error)
      !! The invariant, against a real run rather than the pool in isolation.
      !!
      !! Eight arrivals through a two-stand airport whose apron has a single
      !! entrance: every aircraft passes through node 2, so if reservations did
      !! nothing they would pass through it together.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(id_k) :: aircraft
      integer(int32) :: i
      integer(tick_k) :: at
      integer(id_k) :: holder

      call sim%init(42_int64, err)
      call sim%world%reserve(8_default_int, 2_int32, 1_int32, err)
      call sim%world%graph%build(4_int32, [1_id_k, 2_id_k, 2_id_k], [2_id_k, 3_id_k, 4_id_k], &
                                 [30000_int32, 40000_int32, 40000_int32], err)
      call sim%world%reservations%reserve_pool(4_int32, 512_default_int, err)
      allocate (sim%world%graph%x_cm(4), sim%world%graph%y_cm(4))
      sim%world%graph%x_cm = 0_int32
      sim%world%graph%y_cm = 0_int32

      do i = 1_int32, 2_int32
         sim%world%gate_node(i) = int(2_int32 + i, id_k)
         sim%world%gate_max_wake(i) = WAKE_SUPER
         sim%world%gate_name(i) = "G"
      end do
      sim%world%runway_threshold(1) = 1_id_k
      sim%world%runway_exit(1) = 2_id_k

      do i = 1_int32, 8_int32
         call sim%world%aircraft%add(aircraft, err)
         sim%world%callsign(aircraft) = "T"
         sim%world%aircraft%wake(aircraft) = WAKE_MEDIUM
         sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
         sim%world%aircraft%node(aircraft) = 1_id_k
         sim%world%aircraft%generation(aircraft) = 1_int32
         sim%world%aircraft%fuel_ms(aircraft) = DEFAULT_FUEL_MS
         call sim%schedule_touchdown(aircraft, 1_id_k, &
                                     6_tick_k*HOUR + int(i, tick_k)*2_tick_k*MINUTE, err)
      end do

      ! Step through the whole day a minute at a time. At every instant, at
      ! most one aircraft may hold the apron entrance.
      at = 6_tick_k*HOUR
      do while (at < 16_tick_k*HOUR)
         call sim%run_until(at, err)
         holder = sim%world%reservations%held_by(2_id_k, at)
         ! `held_by` returns the first match, so a second overlapping claim
         ! would be invisible to it. Asking `is_free` while ignoring the known
         ! holder is what finds a second one: with that aircraft discounted,
         ! the node must be free.
         if (holder /= NO_ID) then
            call check(error, sim%world%reservations%is_free(2_id_k, at, at + 1_tick_k, holder), &
                       "two aircraft held the apron entrance at the same instant")
            if (allocated(error)) return
         end if
         at = at + 1_tick_k*MINUTE
      end do

      call sim%destroy()
   end subroutine test_no_overlap_in_practice

end module test_core_reservations
