! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the departure manager.
module test_sys_departure
   !! The separation matrix finally has a caller, and this is where the claim
   !! that it is honoured gets checked rather than asserted.
   !!
   !! `separation_is_never_violated` is the one that matters. It found a real
   !! bug on its first run: an arrival's touchdown wrote `runway_free_at`
   !! flat, clobbering a reservation a departure had already made, and a Light
   !! rolled fifteen seconds behind a Heavy where the matrix demands three
   !! minutes. Every claimant now moves that reservation forward and never
   !! back.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_command, only: command_t
   use core_event_kinds, only: K_CMD_HOLD_DEPARTURE, K_CMD_RELEASE_DEPARTURE
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_sim, only: sim_t, HOUR, MINUTE, SECOND, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, &
                       PHASE_APPROACH, PHASE_AT_GATE, PHASE_READY, PHASE_DEPARTED
   use sys_arrival, only: WAKE_SEP_SEC
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_sys_departure_tests

   integer(int32), parameter :: N_FLEET = 6_int32
      !! Aircraft in the contention fixtures. Six is enough that the takeoff
      !! queue has a shape, and few enough to read the whole board.
   integer(int32), parameter :: N_GATES = 6_int32
      !! Enough stands that nothing queues for one, so the runway is the only
      !! contended resource and separation is the only thing under test.

contains

   subroutine collect_sys_departure_tests(testsuite)
      !! Register the departure tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("an_arrival_becomes_a_departure", test_round_trip), &
                  new_unittest("the_stand_is_given_back", test_stand_released), &
                  new_unittest("separation_is_never_violated", test_separation), &
                  new_unittest("separation_actually_binds", test_separation_binds), &
                  new_unittest("a_hold_keeps_it_on_stand", test_hold), &
                  new_unittest("a_release_lets_it_go", test_release), &
                  new_unittest("delay_is_ready_to_airborne", test_delay) &
                  ]
   end subroutine collect_sys_departure_tests

   subroutine build(sim, seed, wakes, err)
      !! An airport with six stands and one aircraft per entry in `wakes`.
      !!
      !! Arrivals are spaced five minutes apart, which is wide enough that
      !! landing never contends, so anything the runway does later is the
      !! departure queue's doing.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      integer(int32), intent(in) :: wakes(:)
         !! Wake category per aircraft, in arrival order.
      type(error_t), intent(inout) :: err

      integer(id_k) :: aircraft, edge_from(N_GATES + 1), edge_to(N_GATES + 1)
      integer(int32) :: edge_cost(N_GATES + 1), i, n_nodes

      call sim%init(seed, err)

      ! Node 1 threshold, node 2 runway exit, nodes 3.. stands.
      n_nodes = 2_int32 + N_GATES
      call sim%world%reserve(int(size(wakes), default_int), N_GATES, 1_int32, err)

      edge_from(1) = 1_id_k
      edge_to(1) = 2_id_k
      edge_cost(1) = 30000_int32
      do i = 1_int32, N_GATES
         edge_from(i + 1) = 2_id_k
         edge_to(i + 1) = int(2_int32 + i, id_k)
         edge_cost(i + 1) = 40000_int32
      end do
      call sim%world%graph%build(n_nodes, edge_from, edge_to, edge_cost, err)
      allocate (sim%world%graph%x_cm(n_nodes), sim%world%graph%y_cm(n_nodes))
      sim%world%graph%x_cm = 0_int32
      sim%world%graph%y_cm = 0_int32

      do i = 1_int32, N_GATES
         sim%world%gate_node(i) = int(2_int32 + i, id_k)
         sim%world%gate_max_wake(i) = WAKE_SUPER
         sim%world%gate_name(i) = "G"
      end do
      sim%world%runway_threshold(1) = 1_id_k
      sim%world%runway_exit(1) = 2_id_k
      sim%world%runway_name(1) = "09"

      do i = 1_int32, int(size(wakes), int32)
         call sim%world%aircraft%add(aircraft, err)
         sim%world%callsign(aircraft) = "T"
         sim%world%aircraft%wake(aircraft) = wakes(i)
         sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
         sim%world%aircraft%node(aircraft) = 1_id_k
         sim%world%aircraft%generation(aircraft) = 1_int32
         call sim%schedule_touchdown(aircraft, 1_id_k, &
                                     6_tick_k*HOUR + int(i, tick_k)*5_tick_k*MINUTE, err)
      end do
   end subroutine build

   subroutine test_round_trip(error)
      !! An arrival parks, turns round and leaves.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM], err)
      call sim%run_until(12_tick_k*HOUR, err)
      call check(error,.not. err%has_error(), "the run reported an error")
      if (allocated(error)) return

      call check(error, sim%world%aircraft%phase(1) == PHASE_DEPARTED, "it never departed")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%on_blocks_tick(1) > 0_tick_k, "it never parked")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%airborne_tick(1) > sim%world%aircraft%on_blocks_tick(1), &
                 "it left before it parked")

      call sim%destroy()
   end subroutine test_round_trip

   subroutine test_stand_released(error)
      !! The stand comes back when the departure pushes.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(int32) :: gate

      call build(sim, 42_int64, [WAKE_MEDIUM], err)
      call sim%run_until(12_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%gate(1) == NO_ID, "the aircraft still holds a stand")
      if (allocated(error)) return
      do gate = 1_int32, N_GATES
         call check(error, sim%world%gate_occupant(gate) == NO_ID, &
                    "a stand is still marked occupied after the departure left")
         if (allocated(error)) return
      end do

      call sim%destroy()
   end subroutine test_stand_released

   subroutine test_separation(error)
      !! No two consecutive departures are closer than the matrix allows.
      !!
      !! Six aircraft of mixed size against one runway, so the takeoff queue is
      !! genuinely contended. Checking every consecutive pair rather than a
      !! spot case: the bug this caught was one pair in forty-nine.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(default_int) :: i, j, earlier, later
      integer(tick_k) :: gap, required
      integer(int32) :: order(N_FLEET)

      call build(sim, 42_int64, [WAKE_SUPER, WAKE_LIGHT, WAKE_HEAVY, &
                                 WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY], err)
      call sim%run_until(24_tick_k*HOUR, err)
      call check(error,.not. err%has_error(), "the run reported an error")
      if (allocated(error)) return

      ! Departure order is not arrival order -- turnaround length depends on
      ! size -- so sort by the time each actually left the ground.
      call order_by_airborne(sim, order)

      do i = 1_default_int, 5_default_int
         earlier = int(order(i), default_int)
         later = int(order(i + 1), default_int)
         if (sim%world%aircraft%airborne_tick(earlier) == 0_tick_k) cycle
         if (sim%world%aircraft%airborne_tick(later) == 0_tick_k) cycle

         gap = sim%world%aircraft%airborne_tick(later) - sim%world%aircraft%airborne_tick(earlier)
         required = int(WAKE_SEP_SEC(sim%world%aircraft%wake(earlier), &
                                     sim%world%aircraft%wake(later)), tick_k)*SECOND

         call check(error, gap >= required, "two departures were closer than the matrix allows")
         if (allocated(error)) return
      end do

      call sim%destroy()
   end subroutine test_separation

   subroutine test_separation_binds(error)
      !! The matrix is doing work, not being trivially satisfied.
      !!
      !! A simulation that spaced every departure half an hour apart would pass
      !! `separation_is_never_violated` perfectly and model nothing. Contention
      !! has to be manufactured, and the honest way is the player's own lever:
      !! hold every departure through its turnaround, release them all at one
      !! instant, and they arrive at the holding point together.
      !!
      !! With six Mediums that is six aircraft wanting the same slot, and the
      !! matrix hands them out sixty seconds apart. If separation were not
      !! binding they would all roll at once.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(default_int) :: i, earlier, later
      integer(tick_k) :: gap
      integer(int32) :: order(N_FLEET), aircraft
      integer(tick_k), parameter :: MEDIUM_SEPARATION = 60_tick_k*SECOND

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM, &
                                 WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)

      ! Hold everything before any turnaround can finish.
      call sim%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)
      do aircraft = 1_int32, N_FLEET
         call issue(sim, K_CMD_HOLD_DEPARTURE, int(aircraft, id_k), err)
      end do

      ! Let every turnaround complete with the hold on.
      call sim%run_until(8_tick_k*HOUR, err)
      do aircraft = 1_int32, N_FLEET
         call check(error, sim%world%aircraft%phase(aircraft) == PHASE_READY, &
                    "an aircraft was not held and ready before the release")
         if (allocated(error)) return
      end do

      ! Release them in the same instant.
      do aircraft = 1_int32, N_FLEET
         call issue(sim, K_CMD_RELEASE_DEPARTURE, int(aircraft, id_k), err)
      end do
      call sim%run_until(14_tick_k*HOUR, err)

      call order_by_airborne(sim, order)

      do i = 1_default_int, 5_default_int
         earlier = int(order(i), default_int)
         later = int(order(i + 1), default_int)
         call check(error, sim%world%aircraft%airborne_tick(later) > 0_tick_k, &
                    "a released departure never left")
         if (allocated(error)) return

         gap = sim%world%aircraft%airborne_tick(later) - sim%world%aircraft%airborne_tick(earlier)

         call check(error, gap >= MEDIUM_SEPARATION, &
                    "six departures released together were not separated")
         if (allocated(error)) return
         ! And not merely separated -- separated by exactly what the matrix
         ! says, which is what proves the number came from the matrix.
         call check(error, gap == MEDIUM_SEPARATION, &
                    "the gap was not the separation the matrix specifies")
         if (allocated(error)) return
      end do

      call sim%destroy()
   end subroutine test_separation_binds

   subroutine order_by_airborne(sim, order)
      !! Indices of the first six aircraft, ordered by when they left.
      type(sim_t), intent(in) :: sim
      integer(int32), intent(out) :: order(N_FLEET)
         !! Aircraft handles in takeoff order.

      integer(int32) :: i, j, swap

      do i = 1_int32, N_FLEET
         order(i) = i
      end do
      do i = 1_int32, N_FLEET - 1_int32
         do j = 1_int32, N_FLEET - i
            if (sim%world%aircraft%airborne_tick(order(j)) > &
                sim%world%aircraft%airborne_tick(order(j + 1))) then
               swap = order(j)
               order(j) = order(j + 1)
               order(j + 1) = swap
            end if
         end do
      end do
   end subroutine order_by_airborne

   subroutine issue(sim, command_kind, aircraft, err)
      !! Submit a one-argument command.
      !!
      !! The dummy is `command_kind` rather than `kind`, which would shadow the
      !! intrinsic of that name inside this procedure.
      type(sim_t), intent(inout) :: sim
      integer(int16), intent(in) :: command_kind
         !! One of the `K_CMD_*` identifiers.
      integer(id_k), intent(in) :: aircraft
         !! Subject.
      type(error_t), intent(inout) :: err

      type(command_t) :: command

      command%kind = command_kind
      command%a = aircraft
      call sim%submit(command, err)
   end subroutine issue

   subroutine test_hold(error)
      !! A held departure keeps its stand and does not go.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM], err)

      ! During the turnaround, so the hold is in place before pushback is
      ! ever requested. A Medium turns round in about 35 minutes, so any
      ! later than this and there is nothing left to hold.
      call sim%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)
      call issue(sim, K_CMD_HOLD_DEPARTURE, 1_id_k, err)
      call sim%run_until(12_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%held(1) /= 0_int32, "the hold flag was not set")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%phase(1) /= PHASE_DEPARTED, &
                 "a held departure left anyway")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%gate(1) /= NO_ID, &
                 "a held departure gave up its stand")

      call sim%destroy()
   end subroutine test_hold

   subroutine test_release(error)
      !! A released departure goes.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM], err)
      call sim%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)
      call issue(sim, K_CMD_HOLD_DEPARTURE, 1_id_k, err)
      call sim%run_until(9_tick_k*HOUR, err)
      call check(error, sim%world%aircraft%phase(1) /= PHASE_DEPARTED, "it left while held")
      if (allocated(error)) return

      call issue(sim, K_CMD_RELEASE_DEPARTURE, 1_id_k, err)
      call sim%run_until(14_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%held(1) == 0_int32, "the hold flag was not cleared")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%phase(1) == PHASE_DEPARTED, &
                 "a released departure never left")

      call sim%destroy()
   end subroutine test_release

   subroutine test_delay(error)
      !! Delay counts from ready to airborne, and a hold shows up in it.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: free_running, held
      type(error_t) :: err

      call build(free_running, 42_int64, [WAKE_MEDIUM], err)
      call free_running%run_until(14_tick_k*HOUR, err)

      call build(held, 42_int64, [WAKE_MEDIUM], err)
      call held%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)
      call issue(held, K_CMD_HOLD_DEPARTURE, 1_id_k, err)
      call held%run_until(9_tick_k*HOUR, err)
      call issue(held, K_CMD_RELEASE_DEPARTURE, 1_id_k, err)
      call held%run_until(14_tick_k*HOUR, err)

      call check(error, free_running%world%aircraft%delay_ms(1) > 0_tick_k, &
                 "an undelayed departure recorded no delay at all")
      if (allocated(error)) return
      call check(error, held%world%aircraft%delay_ms(1) > free_running%world%aircraft%delay_ms(1), &
                 "holding a departure did not increase its delay")

      call free_running%destroy()
      call held%destroy()
   end subroutine test_delay

end module test_sys_departure
