! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The simulation: the only module `app/` may use.
module core_sim
   !! Owns the world, the queue, the bus, the log and the systems, and runs the
   !! loop that ties them together.
   !!
   !! `app/` uses this module and nothing else from `core/`. That single rule
   !! is what keeps a graphical client from being a rewrite, and CI greps for
   !! violations rather than trusting anyone to remember it.
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_aircraft, only: AIRCRAFT_ROUTE_ROWS
   use core_bus, only: bus_t
   use core_event, only: event_t
   use core_event_kinds, only: K_TOUCHDOWN, K_ROLLOUTCOMPLETE, K_RUNWAYEXITED, &
                               K_TAXINODEREACHED, K_ONBLOCKS, K_GATEASSIGNED, &
                               K_REPLANREQUESTED, event_name
   use core_graph, only: NODE_INTERSECTION, NODE_GATE, NODE_HOLD_SHORT, &
                         NODE_RUNWAY_THRESHOLD, NODE_RUNWAY_EXIT, NODE_DEICE_PAD, &
                         node_kind_name
   use core_log, only: event_log_t
   use core_query, only: aircraft_view_t, gate_view_t, query_aircraft, query_gates
   use core_rng, only: core_stream_for, STREAM_ARRIVAL
   use core_scheduler, only: scheduler_t
   use core_time, only: MILLISECOND, SECOND, MINUTE, HOUR, DAY, tick_split
   use core_world, only: world_t, CALLSIGN_LEN, phase_name, wake_letter, &
                         PHASE_INBOUND, PHASE_APPROACH, PHASE_LANDING, PHASE_ROLLOUT, &
                         PHASE_TAXI_IN, PHASE_AT_GATE, PHASE_DIVERTED, &
                         WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   use sys_arrival, only: arrival_system_t
   use sys_gates, only: gate_system_t
   use sys_taxi, only: taxi_system_t
   use pic_types, only: default_int
   use pic_error, only: error_t
   implicit none
   private

   public :: sim_t

   ! ---- the façade ----------------------------------------------------------
   !
   ! `app/` uses this module and nothing else from `core/`, so everything it
   ! legitimately needs is re-exported here. That is what makes the CI grep a
   ! real rule rather than a suggestion: a new `use core_something` in `app/`
   ! is a build-breaking mistake, not a style preference, because the day a
   ! graphical client arrives it will have exactly this surface to write
   ! against.
   public :: tick_k, id_k, NO_ID, AIRCRAFT_ROUTE_ROWS
   public :: MILLISECOND, SECOND, MINUTE, HOUR, DAY, tick_split
   public :: CALLSIGN_LEN, phase_name, wake_letter, node_kind_name, event_name
   public :: PHASE_INBOUND, PHASE_APPROACH, PHASE_LANDING, PHASE_ROLLOUT
   public :: PHASE_TAXI_IN, PHASE_AT_GATE, PHASE_DIVERTED
   public :: WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   public :: NODE_INTERSECTION, NODE_GATE, NODE_HOLD_SHORT
   public :: NODE_RUNWAY_THRESHOLD, NODE_RUNWAY_EXIT, NODE_DEICE_PAD
   public :: aircraft_view_t, gate_view_t, query_aircraft, query_gates

   type :: sim_t
      !! One simulation.
      type(world_t) :: world
         !! All canonical state.
      type(scheduler_t) :: sched
         !! The event queue.
      type(bus_t) :: bus
         !! The handler table.
      type(event_log_t) :: log
         !! The determinism digest.
      integer(int64) :: seed = 0_int64
         !! Master seed. With the command log, this reproduces the session.

      type(arrival_system_t) :: arrival
      type(gate_system_t) :: gates
      type(taxi_system_t) :: taxi
         !! The systems. Held by value here and referred to by pointer from the
         !! bus, so their lifetime is the simulation's.
   contains
      procedure :: init => sim_init
      procedure :: run_until => sim_run_until
      procedure :: run => sim_run
      procedure :: schedule_touchdown => sim_schedule_touchdown
      procedure :: destroy => sim_destroy
   end type sim_t

contains

   subroutine sim_init(this, seed, err)
      !! Seed the streams and wire every system to its events.
      !!
      !! **The actual argument must have the `target` attribute.** The bus
      !! holds pointers to the systems inside `this`, and a component of a
      !! non-target object is not a valid pointer target. Declaring
      !! `type(sim_t), target :: sim` at the call site is the whole of the
      !! requirement.
      class(sim_t), target, intent(inout) :: this
      integer(int64), intent(in) :: seed
         !! Master seed for the session.
      type(error_t), intent(inout), optional :: err

      this%seed = seed
      call core_stream_for(seed, STREAM_ARRIVAL, this%arrival%rng)

      call this%bus%clear()

      call this%bus%subscribe(K_TOUCHDOWN, this%arrival, err)
      call this%bus%subscribe(K_ROLLOUTCOMPLETE, this%arrival, err)

      call this%bus%subscribe(K_RUNWAYEXITED, this%gates, err)
      call this%bus%subscribe(K_REPLANREQUESTED, this%gates, err)
      call this%bus%subscribe(K_ONBLOCKS, this%gates, err)

      call this%bus%subscribe(K_GATEASSIGNED, this%taxi, err)
      call this%bus%subscribe(K_TAXINODEREACHED, this%taxi, err)
   end subroutine sim_init

   subroutine sim_schedule_touchdown(this, aircraft, runway, at, err)
      !! Put an arrival on the runway at a given sim time.
      !!
      !! This is milestone 0's only entry point into the queue. Milestone 1
      !! replaces it with a schedule loader and player commands, both of which
      !! arrive the same way: as events stamped for a tick, never as a direct
      !! mutation.
      class(sim_t), intent(inout) :: this
      integer(id_k), intent(in) :: aircraft
         !! Aircraft handle.
      integer(id_k), intent(in) :: runway
         !! Runway it lands on.
      integer(tick_k), intent(in) :: at
         !! Sim time of touchdown.
      type(error_t), intent(inout), optional :: err

      call this%sched%push(at=at, kind=K_TOUCHDOWN, entity=aircraft, &
                           generation=this%world%aircraft%generation(aircraft), &
                           payload=int(runway, int64), err=err)
   end subroutine sim_schedule_touchdown

   subroutine sim_run_until(this, horizon, err)
      !! Dispatch every event up to and including `horizon`.
      !!
      !! This is the whole engine. Everything else is systems.
      class(sim_t), intent(inout) :: this
      integer(tick_k), intent(in) :: horizon
         !! Sim time to stop at.
      type(error_t), intent(inout), optional :: err

      type(event_t) :: event

      do while (.not. this%sched%is_empty())
         if (this%sched%next_tick() > horizon) exit

         call this%sched%pop(event, err)
         if (present(err)) then
            if (err%has_error()) return
         end if

         this%world%now = event%tick

         if (is_stale(this%world, event)) then
            call this%log%record_stale()
            cycle
         end if

         call this%log%record(event)
         call this%bus%dispatch(this%world, this%sched, event)
      end do

      this%world%now = horizon
   end subroutine sim_run_until

   subroutine sim_run(this, err)
      !! Dispatch until the queue is empty.
      class(sim_t), intent(inout) :: this
      type(error_t), intent(inout), optional :: err

      call this%run_until(huge(0_tick_k), err)
   end subroutine sim_run

   pure function is_stale(w, event) result(stale)
      !! Whether an event was invalidated after it was scheduled.
      !!
      !! Deleting from the middle of a binary heap is possible and unpleasant.
      !! Instead every aircraft carries a generation counter, events are
      !! stamped with the generation current when they were scheduled, and
      !! bumping the counter invalidates all of an aircraft's pending events at
      !! once. Constant time, however many there are. The cost is a few dead
      !! entries in the queue, which at this scale is nothing.
      type(world_t), intent(in) :: w
      type(event_t), intent(in) :: event
         !! The event about to dispatch.
      logical :: stale

      stale = .false.
      if (event%generation == 0_int32) return
      if (event%entity < 1_id_k) return
      if (int(event%entity, default_int) > w%aircraft%size()) return

      stale = w%aircraft%generation(event%entity) /= event%generation
   end function is_stale

   subroutine sim_destroy(this)
      !! Release everything the simulation owns.
      class(sim_t), intent(inout) :: this

      call this%bus%clear()
      call this%sched%destroy()
      call this%world%destroy()
      call this%log%reset()
   end subroutine sim_destroy

end module core_sim
