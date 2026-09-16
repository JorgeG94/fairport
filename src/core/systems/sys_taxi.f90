! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The taxi system: routing and ground movement.
module sys_taxi
   !! Sole writer of: `aircraft%route`, `aircraft%route_len`,
   !! `aircraft%route_pos`, `aircraft%node`, and `aircraft%phase` while taxiing.
   !!
   !! There is no conflict resolution here yet. Milestone 0 moves one arrival
   !! at a time, so the reservation table that milestone 1 needs would have
   !! nothing to reserve against. What is here is the part that does not
   !! change: routing is integer, the route is committed once, and movement is
   !! one scheduled event per taxiway node rather than a per-tick poll.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_aircraft, only: AIRCRAFT_ROUTE_ROWS
   use core_event, only: event_t
   use core_event_kinds, only: K_GATEASSIGNED, K_TAXINODEREACHED, K_ONBLOCKS, &
                               K_PUSHBACKCOMPLETE, K_LINEUPREQUESTED
   use core_graph, only: taxi_graph_t
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t, PHASE_TAXI_IN, PHASE_TAXI_OUT, PHASE_LINEUP
   use pic_types, only: default_int
   implicit none
   private

   public :: taxi_system_t

   type, extends(system_t) :: taxi_system_t
      !! Route planning and node-to-node movement.
   contains
      procedure :: handle => taxi_handle
      procedure :: tick_order => taxi_tick_order
      procedure :: name => taxi_name
   end type taxi_system_t

contains

   pure function taxi_tick_order(self) result(order)
      !! Runs last of the three: it consumes what the others decided.
      class(taxi_system_t), intent(in) :: self
      integer(int32) :: order

      order = 20_int32
   end function taxi_tick_order

   pure function taxi_name(self) result(name)
      !! Short name for logs.
      class(taxi_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "taxi"
   end function taxi_name

   subroutine taxi_handle(self, w, sched, event)
      !! React to the taxi events.
      class(taxi_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      select case (event%kind)
      case (K_GATEASSIGNED, K_PUSHBACKCOMPLETE)
         ! Inbound and outbound are the same problem: route from where the
         ! aircraft is to wherever it has been told to go. What differs is who
         ! set the goal and what happens on arrival.
         call plan_route(w, sched, event)
      case (K_TAXINODEREACHED)
         call advance(w, sched, event)
      case default
         ! Not ours.
      end select
   end subroutine taxi_handle

   subroutine plan_route(w, sched, event)
      !! Commit a route from where the aircraft is to its assigned stand.
      !!
      !! An aircraft only commits to a route it can actually complete. Where
      !! the plan turns out to be a poor one, that is not a bug -- it is the
      !! delay the player has to manage.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, route(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: route_len
      integer(int64) :: cost

      aircraft = event%entity
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return

      call w%graph%shortest_path(w%aircraft%node(aircraft), w%aircraft%goal(aircraft), &
                                 route, route_len, cost)
      if (route_len <= 0_int32) return

      w%aircraft%route(1:route_len, aircraft) = route(1:route_len)
      w%aircraft%route_len(aircraft) = route_len
      w%aircraft%route_pos(aircraft) = 1_int32
      if (event%kind == K_PUSHBACKCOMPLETE) then
         w%aircraft%phase(aircraft) = PHASE_TAXI_OUT
      else
         w%aircraft%phase(aircraft) = PHASE_TAXI_IN
      end if

      call schedule_next_hop(w, sched, aircraft)
   end subroutine plan_route

   subroutine advance(w, sched, event)
      !! One taxiway node reached. Move on, or park.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft
      integer(int32) :: position

      aircraft = event%entity
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return

      position = w%aircraft%route_pos(aircraft) + 1_int32
      if (position > w%aircraft%route_len(aircraft)) return

      w%aircraft%route_pos(aircraft) = position
      w%aircraft%node(aircraft) = w%aircraft%route(position, aircraft)

      if (position >= w%aircraft%route_len(aircraft)) then
         if (w%aircraft%phase(aircraft) == PHASE_TAXI_OUT) then
            ! At the holding point. Whether it may roll is the departure
            ! manager's call, because the answer depends on wake separation
            ! from whatever used the runway last.
            w%aircraft%phase(aircraft) = PHASE_LINEUP
            call sched%push(at=w%now, kind=K_LINEUPREQUESTED, entity=aircraft, &
                            generation=w%aircraft%generation(aircraft))
         else
            call sched%push(at=w%now, kind=K_ONBLOCKS, entity=aircraft, &
                            generation=w%aircraft%generation(aircraft))
         end if
         return
      end if

      call schedule_next_hop(w, sched, aircraft)
   end subroutine advance

   subroutine schedule_next_hop(w, sched, aircraft)
      !! Schedule arrival at the next node on the committed route.
      type(world_t), intent(in) :: w
      type(scheduler_t), intent(inout) :: sched
      integer(id_k), intent(in) :: aircraft
         !! Aircraft to move.

      integer(int32) :: position, hop_ms
      integer(id_k) :: here, there

      position = w%aircraft%route_pos(aircraft)
      if (position >= w%aircraft%route_len(aircraft)) return

      here = w%aircraft%route(position, aircraft)
      there = w%aircraft%route(position + 1, aircraft)
      hop_ms = edge_cost_between(w%graph, here, there)
      if (hop_ms < 0_int32) return

      call sched%push(at=w%now + int(hop_ms, tick_k), kind=K_TAXINODEREACHED, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(there, int64))
   end subroutine schedule_next_hop

   pure function edge_cost_between(graph, from_node, to_node) result(cost)
      !! Taxi time in milliseconds along the edge joining two nodes.
      !!
      !! A linear scan of one CSR row. Taxiway nodes have a handful of
      !! neighbours each, so this is faster than anything with an index, and it
      !! keeps the graph to three arrays.
      type(taxi_graph_t), intent(in) :: graph
      integer(id_k), intent(in) :: from_node
         !! Node the aircraft is leaving.
      integer(id_k), intent(in) :: to_node
         !! Node it is heading for; must be adjacent.
      integer(int32) :: cost

      integer(int32) :: which

      cost = -1_int32
      if (from_node < 1_id_k .or. from_node > graph%n_nodes) return

      do which = 1_int32, graph%degree(from_node)
         if (graph%neighbour(from_node, which) == to_node) then
            cost = graph%neighbour_cost(from_node, which)
            return
         end if
      end do
   end function edge_cost_between

end module sys_taxi
