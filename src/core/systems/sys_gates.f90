! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The gate system: stands, occupancy, parking.
module sys_gates
   !! Sole writer of: `gate_occupant`, `aircraft%gate`, `aircraft%goal`, and
   !! `aircraft%phase` once the aircraft is on blocks.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_time, only: SECOND
   use core_event, only: event_t
   use core_command, only: command_t
   use core_event_kinds, only: K_RUNWAYEXITED, K_GATEASSIGNED, K_ONBLOCKS, &
                               K_REPLANREQUESTED, K_CMD_ASSIGN_GATE, K_GATERELEASED
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t, PHASE_AT_GATE
   use pic_types, only: default_int
   implicit none
   private

   public :: gate_system_t

   integer(tick_k), parameter :: GATE_RETRY_MS = 60_tick_k*SECOND
      !! How long an arrival waits before asking for a stand again.
      !!
      !! An arrival with nowhere to park is the first half of the deadlock that
      !! makes disruption interesting, so it waits rather than failing: it is
      !! holding a runway exit while it waits, and that cost should be visible
      !! in the delay figures rather than hidden behind an error.

   type, extends(system_t) :: gate_system_t
      !! Stand assignment and parking.
   contains
      procedure :: handle => gate_handle
      procedure :: tick_order => gate_tick_order
      procedure :: name => gate_name
   end type gate_system_t

contains

   pure function gate_tick_order(self) result(order)
      !! Runs after the arrival manager and before the taxi planner: a stand
      !! has to exist before a route to it can be planned.
      class(gate_system_t), intent(in) :: self
      integer(int32) :: order

      order = 10_int32
   end function gate_tick_order

   pure function gate_name(self) result(name)
      !! Short name for logs.
      class(gate_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "gates"
   end function gate_name

   subroutine gate_handle(self, w, sched, event)
      !! React to the gate events.
      class(gate_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      select case (event%kind)
      case (K_RUNWAYEXITED, K_REPLANREQUESTED)
         call assign_stand(w, sched, event)
      case (K_ONBLOCKS)
         call on_blocks(w, event)
      case (K_CMD_ASSIGN_GATE)
         call commanded_stand(w, sched, event)
      case (K_GATERELEASED)
         call release_stand(w, event)
      case default
         ! Not ours.
      end select
   end subroutine gate_handle

   subroutine assign_stand(w, sched, event)
      !! Find a stand for an arrival, or ask again shortly.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, gate

      aircraft = event%entity
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (w%aircraft%gate(aircraft) /= NO_ID) return

      gate = w%free_gate_for(w%aircraft%wake(aircraft))
      if (gate == NO_ID) then
         ! Ask again only if a compatible stand exists to become free. If none
         ! does, the aircraft stays put and the board shows it: retrying every
         ! minute until the horizon would fill the log with events that were
         ! never going to succeed, and hide the scenario error that caused it.
         if (w%has_stand_for(w%aircraft%wake(aircraft))) then
            call sched%push(at=w%now + GATE_RETRY_MS, kind=K_REPLANREQUESTED, &
                            entity=aircraft, generation=w%aircraft%generation(aircraft))
         end if
         return
      end if

      w%gate_occupant(gate) = aircraft
      w%aircraft%gate(aircraft) = gate
      w%aircraft%goal(aircraft) = w%gate_node(gate)

      call sched%push(at=w%now, kind=K_GATEASSIGNED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft), &
                      payload=int(gate, int64))
   end subroutine assign_stand

   subroutine commanded_stand(w, sched, event)
      !! The player has named a stand for an aircraft.
      !!
      !! A command overrides the tightest-fit choice `free_gate_for` would
      !! have made, which is the point: the solver is deliberately mediocre and
      !! the player is the optimizer. What a command cannot do is break an
      !! invariant -- an occupied stand stays occupied, and a stand too small
      !! for the aircraft stays refused. A rejected command is not an error; it
      !! is a decision that did not work, and it stays in the log so the replay
      !! rejects it identically.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      type(command_t) :: command
      integer(tick_k) :: issued_at
      integer(id_k) :: aircraft, gate

      call w%commands%get(event%payload, issued_at, command)
      aircraft = command%a
      gate = command%b

      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (gate < 1_id_k .or. gate > w%n_gates) return

      ! Already parked, or already heading somewhere: too late to redirect at
      ! milestone 0's fidelity. Re-routing a taxiing aircraft is the
      ! reservation table's problem, and it does not exist yet.
      if (w%aircraft%gate(aircraft) /= NO_ID) return

      if (w%gate_occupant(gate) /= NO_ID) return
      if (w%gate_max_wake(gate) < w%aircraft%wake(aircraft)) return

      w%gate_occupant(gate) = aircraft
      w%aircraft%gate(aircraft) = gate
      w%aircraft%goal(aircraft) = w%gate_node(gate)

      call sched%push(at=w%now, kind=K_GATEASSIGNED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft), &
                      payload=int(gate, int64))
   end subroutine commanded_stand

   subroutine release_stand(w, event)
      !! A departure is clear of its stand.
      !!
      !! Nothing is scheduled to tell waiting arrivals. They are already asking
      !! every minute, and a stand that frees is picked up by the next retry.
      !! Notifying every waiting aircraft instead would mean forty of them
      !! re-deciding at once, which is where both nondeterminism and gridlock
      !! come from.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, gate

      aircraft = event%entity
      gate = int(event%payload, id_k)
      if (gate < 1_id_k .or. gate > w%n_gates) return
      if (w%gate_occupant(gate) /= aircraft) return

      w%gate_occupant(gate) = NO_ID
      if (aircraft >= 1_id_k .and. int(aircraft, default_int) <= w%aircraft%size()) then
         w%aircraft%gate(aircraft) = NO_ID
      end if
   end subroutine release_stand

   subroutine on_blocks(w, event)
      !! Chocks in. The arrival is parked.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft

      aircraft = event%entity
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return

      w%aircraft%phase(aircraft) = PHASE_AT_GATE
      w%aircraft%on_blocks_tick(aircraft) = w%now
   end subroutine on_blocks

end module sys_gates
