! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for wake separation.
module test_sys_arrival
   !! The separation matrix is what makes one runway a puzzle rather than a
   !! queue, and it is the one piece of milestone 0 that nothing yet calls.
   !!
   !! That is deliberate: separation constrains *sequencing*, and milestone 0
   !! has no sequencer to constrain -- arrivals land at the times the scenario
   !! names. Rather than delete the table and rewrite it for milestone 1, it is
   !! tested here against the properties the design document states, so that
   !! when the departure manager arrives it is wiring up something already
   !! known to be right.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use sys_arrival, only: WAKE_SEP_SEC, earliest_slot
   use core_kinds, only: tick_k, id_k, int32
   use core_time, only: SECOND
   use core_world, only: world_t, WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_sys_arrival_tests

contains

   subroutine collect_sys_arrival_tests(testsuite)
      !! Register the separation tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("matrix_matches_the_design", test_matrix_values), &
                  new_unittest("a_bigger_leader_costs_more", test_leader_monotonic), &
                  new_unittest("a_smaller_follower_costs_more", test_follower_monotonic), &
                  new_unittest("separation_is_not_symmetric", test_asymmetry), &
                  new_unittest("slot_respects_separation", test_earliest_slot), &
                  new_unittest("slot_never_precedes_now", test_slot_not_in_past) &
                  ]
   end subroutine collect_sys_arrival_tests

   subroutine test_matrix_values(error)
      !! Spot-check the table against the figures in the design document.
      type(error_type), allocatable, intent(out) :: error

      call check(error, WAKE_SEP_SEC(WAKE_LIGHT, WAKE_LIGHT) == 60_int32, "light behind light")
      if (allocated(error)) return
      call check(error, WAKE_SEP_SEC(WAKE_SUPER, WAKE_LIGHT) == 240_int32, "light behind super")
      if (allocated(error)) return
      call check(error, WAKE_SEP_SEC(WAKE_HEAVY, WAKE_MEDIUM) == 120_int32, "medium behind heavy")
      if (allocated(error)) return
      call check(error, WAKE_SEP_SEC(WAKE_MEDIUM, WAKE_HEAVY) == 60_int32, "heavy behind medium")
      if (allocated(error)) return
      call check(error, WAKE_SEP_SEC(WAKE_SUPER, WAKE_SUPER) == 90_int32, "super behind super")
   end subroutine test_matrix_values

   subroutine test_leader_monotonic(error)
      !! For a fixed follower, a larger leader never requires less spacing.
      !!
      !! The table is written row-major in the source with `order=[2, 1]`. If
      !! that reshape were wrong the table would transpose, and this is one of
      !! the two properties that would catch it.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: leader, follower

      do follower = WAKE_LIGHT, WAKE_SUPER
         do leader = WAKE_LIGHT, WAKE_SUPER - 1_int32
            call check(error, WAKE_SEP_SEC(leader, follower) <= WAKE_SEP_SEC(leader + 1_int32, follower), &
                       "a larger leader required less separation")
            if (allocated(error)) return
         end do
      end do
   end subroutine test_leader_monotonic

   subroutine test_follower_monotonic(error)
      !! For a fixed leader, a larger follower never requires more spacing.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: leader, follower

      do leader = WAKE_LIGHT, WAKE_SUPER
         do follower = WAKE_LIGHT, WAKE_SUPER - 1_int32
            call check(error, WAKE_SEP_SEC(leader, follower) >= WAKE_SEP_SEC(leader, follower + 1_int32), &
                       "a larger follower required more separation")
            if (allocated(error)) return
         end do
      end do
   end subroutine test_follower_monotonic

   subroutine test_asymmetry(error)
      !! Order matters, which is the entire point.
      !!
      !! If separation depended only on the pair rather than on which came
      !! first, sequencing would not change throughput and there would be no
      !! decision for the player to make.
      type(error_type), allocatable, intent(out) :: error

      call check(error, WAKE_SEP_SEC(WAKE_SUPER, WAKE_LIGHT) /= WAKE_SEP_SEC(WAKE_LIGHT, WAKE_SUPER), &
                 "the matrix is symmetric, so ordering would not matter")
      if (allocated(error)) return
      call check(error, WAKE_SEP_SEC(WAKE_SUPER, WAKE_LIGHT) > WAKE_SEP_SEC(WAKE_LIGHT, WAKE_SUPER), &
                 "a light behind a super should wait longer than the reverse")
   end subroutine test_asymmetry

   subroutine test_earliest_slot(error)
      !! The next slot is the runway's free time plus the pair's separation.
      type(error_type), allocatable, intent(out) :: error

      type(world_t) :: w
      type(error_t) :: err
      integer(tick_k) :: slot

      call w%reserve(1_default_int, 1_int32, 1_int32, err)
      w%now = 0_tick_k
      w%runway_free_at(1) = 1000_tick_k*SECOND
      w%runway_last_wake(1) = WAKE_SUPER

      slot = earliest_slot(w, 1_id_k, WAKE_LIGHT)
      call check(error, slot == 1000_tick_k*SECOND + 240_tick_k*SECOND, &
                 "a light behind a super waits four minutes")
      if (allocated(error)) return

      w%runway_last_wake(1) = WAKE_MEDIUM
      slot = earliest_slot(w, 1_id_k, WAKE_HEAVY)
      call check(error, slot == 1000_tick_k*SECOND + 60_tick_k*SECOND, &
                 "a heavy behind a medium waits one minute")

      call w%destroy()
   end subroutine test_earliest_slot

   subroutine test_slot_not_in_past(error)
      !! A slot that has already passed is now, never earlier.
      type(error_type), allocatable, intent(out) :: error

      type(world_t) :: w
      type(error_t) :: err
      integer(tick_k) :: slot

      call w%reserve(1_default_int, 1_int32, 1_int32, err)
      w%now = 5000_tick_k*SECOND
      w%runway_free_at(1) = 0_tick_k
      w%runway_last_wake(1) = WAKE_LIGHT

      slot = earliest_slot(w, 1_id_k, WAKE_LIGHT)
      call check(error, slot == w%now, "a stale slot was returned in the past")

      call w%destroy()
   end subroutine test_slot_not_in_past

end module test_sys_arrival
