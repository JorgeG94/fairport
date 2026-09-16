! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the landing order and the takeoff queue.
module test_sequencing
   !! Sequencing is the last of the milestone 1 player actions, and the one
   !! that matters most once holding is scarce: when there is not enough runway
   !! for everybody, the order is how you choose who diverts.
   !!
   !! Two bugs turned up while writing these, both worth recording:
   !!
   !! - Making the asking aircraft defer to the front of the stack wasted the
   !!   slot, because the front one would not ask again for a full circuit. On
   !!   a busy day that cost two extra diversions. The controller now hands the
   !!   clearance to the front aircraft directly.
   !! - The "is there anywhere to park it" check was done for whoever asked and
   !!   the clearance then went to whoever was in front, so a free Medium stand
   !!   could clear a Super. It is now asked per aircraft.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_command, only: command_t
   use core_event_kinds, only: K_CMD_SEQUENCE_ARRIVAL, K_CMD_SEQUENCE_DEPARTURE, &
                               K_CMD_HOLD_DEPARTURE, K_CMD_RELEASE_DEPARTURE, &
                               K_CMD_SET_VISIBILITY
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_sim, only: sim_t, HOUR, MINUTE, SECOND, DEFAULT_FUEL_MS, &
                       WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, &
                       PHASE_APPROACH, PHASE_HOLDING, PHASE_DIVERTED, PHASE_READY
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_sequencing_tests

   integer(int32), parameter :: MAX_STANDS = 8_int32
      !! Upper bound on the fixtures' edge arrays, so the sizes are named
      !! rather than guessed at each call site.

   integer(int32), parameter :: N_STANDS = 1_int32
      !! Exactly one, so that only one arrival at a time can be taken and the
      !! order is the only thing deciding who it is.

contains

   subroutine collect_sequencing_tests(testsuite)
      !! Register the sequencing tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("unsequenced_is_longest_waiting_first", test_fifo_default), &
                  new_unittest("sequencing_beats_waiting_longer", test_arrival_priority), &
                  new_unittest("sequencing_changes_who_diverts", test_who_diverts), &
                  new_unittest("zero_puts_it_back_in_the_pack", test_unsequence), &
                  new_unittest("a_clearance_is_never_wasted", test_no_wasted_slot), &
                  new_unittest("a_stand_must_fit_the_aircraft_cleared", test_stand_fits), &
                  new_unittest("departure_order_follows_the_player", test_departure_order) &
                  ]
   end subroutine collect_sequencing_tests

   subroutine build(sim, seed, wakes, err)
      !! A one-stand airport: the scarcest possible, so the landing order is
      !! the only thing deciding who gets in.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      integer(int32), intent(in) :: wakes(:)
         !! Wake category per aircraft, in arrival order.
      type(error_t), intent(inout) :: err

      call build_with_stands(sim, seed, wakes, N_STANDS, err)
   end subroutine build

   subroutine build_with_stands(sim, seed, wakes, n_stands, err)
      !! An airport with `n_stands` stands and an arrival per entry in `wakes`,
      !! one minute apart.
      !!
      !! The departure tests need two. With one, holding a departure on it
      !! means the next arrival never gets a stand, never turns round and never
      !! becomes a departure -- so the test would be reordering a queue with
      !! one aircraft in it.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      integer(int32), intent(in) :: wakes(:)
         !! Wake category per aircraft, in arrival order.
      integer(int32), intent(in) :: n_stands
         !! Stands on the field.
      type(error_t), intent(inout) :: err

      integer(id_k) :: aircraft, edge_from(MAX_STANDS), edge_to(MAX_STANDS)
      integer(int32) :: edge_cost(MAX_STANDS), i, n_nodes

      call sim%init(seed, err)
      n_nodes = 2_int32 + n_stands
      call sim%world%reserve(int(size(wakes), default_int), n_stands, 1_int32, err)

      edge_from(1) = 1_id_k
      edge_to(1) = 2_id_k
      edge_cost(1) = 30000_int32
      do i = 1_int32, n_stands
         edge_from(i + 1) = 2_id_k
         edge_to(i + 1) = int(2_int32 + i, id_k)
         edge_cost(i + 1) = 40000_int32
      end do
      call sim%world%graph%build(n_nodes, edge_from(1:n_stands + 1), &
                                 edge_to(1:n_stands + 1), edge_cost(1:n_stands + 1), err)
      call sim%world%reservations%reserve_pool(n_nodes, &
                                               64_default_int*sim%world%aircraft%capacity(), err)
      allocate (sim%world%graph%x_cm(n_nodes), sim%world%graph%y_cm(n_nodes))
      sim%world%graph%x_cm = 0_int32
      sim%world%graph%y_cm = 0_int32

      do i = 1_int32, n_stands
         sim%world%gate_node(i) = int(2_int32 + i, id_k)
         sim%world%gate_max_wake(i) = WAKE_SUPER
         sim%world%gate_name(i) = "A"
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
         sim%world%aircraft%fuel_ms(aircraft) = DEFAULT_FUEL_MS
         call sim%schedule_touchdown(aircraft, 1_id_k, &
                                     6_tick_k*HOUR + int(i, tick_k)*1_tick_k*MINUTE, err)
      end do
   end subroutine build_with_stands

   subroutine sequence(sim, command_kind, aircraft, position, err)
      !! Submit a two-argument sequencing command.
      type(sim_t), intent(inout) :: sim
      integer(int16), intent(in) :: command_kind
         !! One of the `K_CMD_SEQUENCE_*` identifiers.
      integer(id_k), intent(in) :: aircraft
         !! Subject.
      integer(id_k), intent(in) :: position
         !! Place in the order; 1 is next, 0 is unsequenced.
      type(error_t), intent(inout) :: err

      type(command_t) :: command

      command%kind = command_kind
      command%a = aircraft
      command%b = position
      call sim%submit(command, err)
   end subroutine sequence

   subroutine visibility(sim, metres, err)
      !! Report a visibility.
      type(sim_t), intent(inout) :: sim
      integer(id_k), intent(in) :: metres
         !! Visibility in metres.
      type(error_t), intent(inout) :: err

      type(command_t) :: command

      command%kind = K_CMD_SET_VISIBILITY
      command%a = metres
      call sim%submit(command, err)
   end subroutine visibility

   subroutine hold(sim, command_kind, aircraft, err)
      !! Submit a one-argument departure command.
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
   end subroutine hold

   subroutine test_fifo_default(error)
      !! With nobody sequenced, the aircraft holding longest lands first.
      !!
      !! This is what stops the unsequenced majority starving while the player
      !! attends to two aircraft, and it is why a busy day loses three rather
      !! than eight.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)
      call sim%run_until(7_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%touchdown_tick(1) > 0_tick_k, &
                 "the first to arrive did not land first")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) < &
                 max(sim%world%aircraft%touchdown_tick(2), 1_tick_k), &
                 "landing order did not follow arrival order")

      call sim%destroy()
   end subroutine test_fifo_default

   subroutine test_arrival_priority(error)
      !! A sequenced aircraft goes before one that has been waiting longer.
      !!
      !! The airport is shut until all three are in the stack. Sequencing can
      !! only reorder aircraft that are actually holding -- a slot cannot be
      !! kept warm for one that has not arrived -- so the closure is how the
      !! test gets all three into the queue before anybody is cleared.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)

      call sim%run_until(5_tick_k*HOUR, err)
      call visibility(sim, 400_id_k, err)
      call sim%run_until(6_tick_k*HOUR + 10_tick_k*MINUTE, err)

      call sequence(sim, K_CMD_SEQUENCE_ARRIVAL, 3_id_k, 1_id_k, err)
      call visibility(sim, 9000_id_k, err)
      call sim%run_until(7_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%touchdown_tick(3) > 0_tick_k, &
                 "the sequenced aircraft never landed")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%arr_sequence(3) == 1_int32, &
                 "the sequence position was not recorded")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) == 0_tick_k .or. &
                 sim%world%aircraft%touchdown_tick(3) < sim%world%aircraft%touchdown_tick(1), &
                 "the sequenced aircraft did not go first")

      call sim%destroy()
   end subroutine test_arrival_priority

   subroutine test_who_diverts(error)
      !! Sequencing changes which aircraft is lost, not how many.
      !!
      !! One stand and three arrivals is not enough for everybody, and no
      !! ordering invents a stand. What the player chooses is who survives.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: unattended, sequenced
      type(error_t) :: err
      integer(default_int) :: i, lost_unattended, lost_sequenced

      ! Six against one stand. Three is not enough, because the apron buffer
      ! absorbs two of them and nobody is lost -- which is the buffer doing its
      ! job, and a fixture that does not test what it claims to.
      call build(unattended, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM, &
                                        WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)
      call unattended%run_until(10_tick_k*HOUR, err)

      call build(sequenced, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM, &
                                       WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)
      call sequenced%run_until(6_tick_k*HOUR, err)
      call sequence(sequenced, K_CMD_SEQUENCE_ARRIVAL, 6_id_k, 1_id_k, err)
      call sequenced%run_until(10_tick_k*HOUR, err)

      lost_unattended = 0_default_int
      lost_sequenced = 0_default_int
      do i = 1_default_int, 6_default_int
         if (unattended%world%aircraft%phase(i) == PHASE_DIVERTED) then
            lost_unattended = lost_unattended + 1_default_int
         end if
         if (sequenced%world%aircraft%phase(i) == PHASE_DIVERTED) then
            lost_sequenced = lost_sequenced + 1_default_int
         end if
      end do

      call check(error, lost_unattended > 0_default_int, &
                 "one stand and three arrivals lost nobody, so the fixture is not tight enough")
      if (allocated(error)) return
      call check(error, sequenced%world%aircraft%phase(6) /= PHASE_DIVERTED, &
                 "the aircraft put at the front of the order was lost anyway")
      if (allocated(error)) return
      call check(error, sequenced%log%digest() /= unattended%log%digest(), &
                 "the sequencing command changed nothing about the run")

      call sequenced%destroy()
      call unattended%destroy()
   end subroutine test_who_diverts

   subroutine test_unsequence(error)
      !! Position zero returns an aircraft to the unsequenced majority.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM], err)
      call sim%run_until(6_tick_k*HOUR, err)
      call sequence(sim, K_CMD_SEQUENCE_ARRIVAL, 2_id_k, 3_id_k, err)
      call sim%run_until(6_tick_k*HOUR + 1_tick_k*MINUTE, err)
      call check(error, sim%world%aircraft%arr_sequence(2) == 3_int32, "position was not set")
      if (allocated(error)) return

      call sequence(sim, K_CMD_SEQUENCE_ARRIVAL, 2_id_k, 0_id_k, err)
      call sim%run_until(6_tick_k*HOUR + 2_tick_k*MINUTE, err)
      call check(error, sim%world%aircraft%arr_sequence(2) == 0_int32, "position was not cleared")

      call sim%destroy()
   end subroutine test_unsequence

   subroutine test_no_wasted_slot(error)
      !! A clearance goes to somebody whenever one is available.
      !!
      !! The first implementation made the asking aircraft defer to the front
      !! of the stack, which meant the slot went unused because the front one
      !! would not ask again for a full circuit. The symptom was more holding
      !! and more diversions, not an obvious failure, so this asserts the
      !! property directly: with a free stand and aircraft waiting, somebody
      !! lands promptly.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err
      integer(default_int) :: i, landed

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM, WAKE_MEDIUM], err)
      call sim%run_until(6_tick_k*HOUR, err)
      call sequence(sim, K_CMD_SEQUENCE_ARRIVAL, 3_id_k, 1_id_k, err)

      ! Ten minutes is two and a half holding circuits. If a slot were being
      ! wasted every time the wrong aircraft asked, nobody would be down yet.
      call sim%run_until(6_tick_k*HOUR + 10_tick_k*MINUTE, err)

      landed = 0_default_int
      do i = 1_default_int, 3_default_int
         if (sim%world%aircraft%touchdown_tick(i) > 0_tick_k) landed = landed + 1_default_int
      end do

      call check(error, landed > 0_default_int, &
                 "nobody landed in ten minutes with a stand free, so a clearance was wasted")

      call sim%destroy()
   end subroutine test_no_wasted_slot

   subroutine test_stand_fits(error)
      !! An aircraft is only cleared onto a stand that takes it.
      !!
      !! The stand check used to be asked for whoever requested the clearance
      !! and the clearance then went to whoever was at the front of the order,
      !! so a free Medium stand could clear a Super onto a field with nowhere
      !! to put it.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, [WAKE_MEDIUM, WAKE_SUPER], err)
      ! The only stand takes Medium and no more.
      sim%world%gate_max_wake(1) = WAKE_MEDIUM

      call sim%run_until(6_tick_k*HOUR, err)
      call sequence(sim, K_CMD_SEQUENCE_ARRIVAL, 2_id_k, 1_id_k, err)
      call sim%run_until(8_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%touchdown_tick(2) == 0_tick_k, &
                 "a Super was cleared onto an airport with no Super stand")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) > 0_tick_k, &
                 "the Medium was blocked by a Super that could never be taken")

      call sim%destroy()
   end subroutine test_stand_fits

   subroutine test_departure_order(error)
      !! The takeoff queue follows the player's order too.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      !! Both are held on their stands until both are ready, then released
      !! together, because a departure cannot be sequenced ahead of one that
      !! has not reached the holding point yet.
      call build_with_stands(sim, 42_int64, [WAKE_MEDIUM, WAKE_MEDIUM], 2_int32, err)

      call sim%run_until(6_tick_k*HOUR + 30_tick_k*MINUTE, err)
      call hold(sim, K_CMD_HOLD_DEPARTURE, 1_id_k, err)
      call hold(sim, K_CMD_HOLD_DEPARTURE, 2_id_k, err)

      call sim%run_until(12_tick_k*HOUR, err)
      call sequence(sim, K_CMD_SEQUENCE_DEPARTURE, 2_id_k, 1_id_k, err)
      call hold(sim, K_CMD_RELEASE_DEPARTURE, 1_id_k, err)
      call hold(sim, K_CMD_RELEASE_DEPARTURE, 2_id_k, err)
      call sim%run_until(20_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%dep_sequence(2) == 1_int32, &
                 "the departure position was not recorded")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%airborne_tick(2) > 0_tick_k, &
                 "the sequenced departure never left")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%airborne_tick(1) == 0_tick_k .or. &
                 sim%world%aircraft%airborne_tick(2) < sim%world%aircraft%airborne_tick(1), &
                 "the sequenced departure did not go first")

      call sim%destroy()
   end subroutine test_departure_order

end module test_sequencing
