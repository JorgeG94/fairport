! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the property the whole design exists to protect.
module test_core_determinism
   !! Reproducible from `(seed, command log)` alone.
   !!
   !! Everything else -- save files, replays, regression tests, cross-platform
   !! CI, bug reports -- falls out of that one property for free. These tests
   !! are the local half of it: same input, same digest, and a different seed
   !! actually changes something. The other half is the cross-compiler fan-in
   !! job, which compares the digests four front ends produce and can only be
   !! run by CI.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_sim, only: sim_t, tick_k, id_k, MINUTE, HOUR, &
                       WAKE_HEAVY, WAKE_MEDIUM, PHASE_APPROACH, PHASE_AT_GATE
   use pic_error, only: error_t
   use pic_types, only: default_int, int32, int64
   implicit none
   private

   public :: collect_core_determinism_tests

contains

   subroutine collect_core_determinism_tests(testsuite)
      !! Register the determinism tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("same_seed_same_digest", test_same_seed), &
                  new_unittest("different_seed_different_digest", test_different_seed), &
                  new_unittest("aircraft_reaches_its_stand", test_reaches_stand), &
                  new_unittest("digest_is_order_sensitive", test_order_sensitive), &
                  new_unittest("stale_events_are_dropped", test_tombstoning) &
                  ]
   end subroutine collect_core_determinism_tests

   subroutine build_toy(sim, seed, err)
      !! A three-node airport: threshold, runway exit, one stand.
      !!
      !! Small enough to reason about by hand, complete enough that an arrival
      !! runs the whole milestone 0 chain: touchdown, rollout, vacate, gate
      !! assignment, taxi, on blocks.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      type(error_t), intent(inout) :: err

      integer(id_k) :: aircraft

      call sim%init(seed, err)
      call sim%world%reserve(4_default_int, 1_int32, 1_int32, err)

      call sim%world%graph%build(3_int32, [1_id_k, 2_id_k], [2_id_k, 3_id_k], &
                                 [30000_int32, 40000_int32], err)
      allocate (sim%world%graph%x_cm(3), sim%world%graph%y_cm(3))
      sim%world%graph%x_cm = 0_int32
      sim%world%graph%y_cm = 0_int32

      sim%world%gate_node(1) = 3_id_k
      sim%world%gate_max_wake(1) = WAKE_HEAVY
      sim%world%gate_name(1) = "A1"
      sim%world%runway_threshold(1) = 1_id_k
      sim%world%runway_exit(1) = 2_id_k
      sim%world%runway_name(1) = "09"

      call sim%world%aircraft%add(aircraft, err)
      sim%world%callsign(aircraft) = "TEST001"
      sim%world%aircraft%wake(aircraft) = WAKE_MEDIUM
      sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
      sim%world%aircraft%node(aircraft) = 1_id_k
      sim%world%aircraft%generation(aircraft) = 1_int32

      call sim%schedule_touchdown(aircraft, 1_id_k, 6_tick_k*HOUR, err)
   end subroutine build_toy

   subroutine test_same_seed(error)
      !! Two runs of the same input agree bit for bit.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: first, second
      type(error_t) :: err

      call build_toy(first, 42_int64, err)
      call first%run_until(9_tick_k*HOUR, err)

      call build_toy(second, 42_int64, err)
      call second%run_until(9_tick_k*HOUR, err)

      call check(error,.not. err%has_error(), "a run reported an error")
      if (allocated(error)) return
      call check(error, first%log%digest() == second%log%digest(), &
                 "same seed produced a different digest")
      if (allocated(error)) return
      call check(error, first%log%count() == second%log%count(), &
                 "same seed dispatched a different number of events")
      if (allocated(error)) return
      call check(error, first%log%count() > 0_int64, "nothing was dispatched at all")

      call first%destroy()
      call second%destroy()
   end subroutine test_same_seed

   subroutine test_different_seed(error)
      !! The seed actually reaches the simulation.
      !!
      !! Without this, `same_seed_same_digest` would pass just as happily on a
      !! simulation that ignored its random stream entirely.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: first, second
      type(error_t) :: err

      call build_toy(first, 42_int64, err)
      call first%run_until(9_tick_k*HOUR, err)

      call build_toy(second, 4242_int64, err)
      call second%run_until(9_tick_k*HOUR, err)

      call check(error, first%log%digest() /= second%log%digest(), &
                 "the seed made no difference to the run")

      call first%destroy()
      call second%destroy()
   end subroutine test_different_seed

   subroutine test_reaches_stand(error)
      !! The milestone 0 acceptance criterion: it lands, it taxis, it parks.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build_toy(sim, 7_int64, err)
      call sim%run_until(9_tick_k*HOUR, err)
      call check(error,.not. err%has_error(), "the run reported an error")
      if (allocated(error)) return

      ! Since the departure manager landed, an aircraft that reaches a stand
      ! also leaves it again, so the evidence is the on-blocks tick rather than
      ! the phase it happens to be in at the horizon.
      call check(error, sim%world%aircraft%on_blocks_tick(1) > 0_tick_k, &
                 "the aircraft never reached a stand")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%on_blocks_tick(1) > sim%world%aircraft%touchdown_tick(1), &
                 "it parked before it landed")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%on_blocks_tick(1) > sim%world%aircraft%touchdown_tick(1), &
                 "it parked before it landed")

      call sim%destroy()
   end subroutine test_reaches_stand

   subroutine test_order_sensitive(error)
      !! Running to a shorter horizon gives a different digest.
      !!
      !! A digest that ignored what actually happened would still match here,
      !! so this is the check that the fold is over the event stream rather
      !! than over nothing.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: full, partial
      type(error_t) :: err

      call build_toy(full, 42_int64, err)
      call full%run_until(9_tick_k*HOUR, err)

      call build_toy(partial, 42_int64, err)
      call partial%run_until(6_tick_k*HOUR + 1_tick_k*MINUTE, err)

      call check(error, full%log%count() > partial%log%count(), &
                 "the shorter run dispatched no fewer events")
      if (allocated(error)) return
      call check(error, full%log%digest() /= partial%log%digest(), &
                 "a shorter run produced the same digest")

      call full%destroy()
      call partial%destroy()
   end subroutine test_order_sensitive

   subroutine test_tombstoning(error)
      !! Bumping a generation invalidates that aircraft's pending events.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build_toy(sim, 42_int64, err)

      ! Advance to just after touchdown, so a rollout event is pending, then
      ! invalidate everything scheduled for the aircraft.
      call sim%run_until(6_tick_k*HOUR, err)
      sim%world%aircraft%generation(1) = sim%world%aircraft%generation(1) + 1_int32
      call sim%run_until(9_tick_k*HOUR, err)

      call check(error, sim%log%stale_count() > 0_int64, "nothing was tombstoned")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%phase(1) /= PHASE_AT_GATE, &
                 "a cancelled aircraft still reached its stand")

      call sim%destroy()
   end subroutine test_tombstoning

end module test_core_determinism
