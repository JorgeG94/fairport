! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The departure manager: turnaround, pushback, and the runway queue.
module sys_departure
   !! Sole writer of: `aircraft%held`, `aircraft%ready_tick`,
   !! `aircraft%airborne_tick`, and `aircraft%phase` from turnaround onwards.
   !! It also advances `runway_free_at` and `runway_last_wake` for departures,
   !! which the arrival manager does for arrivals.
   !!
   !! ## Why this is where the game appears
   !!
   !! Arrivals land when the schedule says. Departures go when the runway lets
   !! them, and the runway's answer depends on what went before: a Light behind
   !! a Super waits four minutes, a Heavy behind a Medium waits one. The
   !! separation matrix has been sitting in `sys_arrival` since milestone 0
   !! with nothing calling it, because separation constrains *sequencing* and
   !! until now there was no sequence. This is the caller.
   !!
   !! The consequence is that the order departures reach the threshold changes
   !! how many of them get away in an hour. That is the decision the player is
   !! there to make, and `hold_departure` is the lever.
   !!
   !! ## The queue is deliberately first-come
   !!
   !! Aircraft take runway slots in the order they arrive at the threshold, and
   !! nothing here reorders them to group wake categories. A solver that did
   !! would be worth perhaps fifteen percent of throughput and would remove the
   !! only interesting decision on the airfield. The player reorders, by
   !! holding one departure so another goes first.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_command, only: command_t
   use core_event, only: event_t
   use core_event_kinds, only: K_ONBLOCKS, K_TURNAROUNDCOMPLETE, K_PUSHBACKREQUESTED, &
                               K_PUSHBACKCOMPLETE, K_GATERELEASED, K_LINEUPCLEARED, &
                               K_TAKEOFFROLLCOMPLETE, K_LINEUPREQUESTED, &
                               K_CMD_HOLD_DEPARTURE, &
                               K_CMD_RELEASE_DEPARTURE
   use core_rng, only: stream_t
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_time, only: SECOND, MINUTE
   use core_world, only: world_t, PHASE_TURNAROUND, PHASE_READY, PHASE_PUSHBACK, &
                         PHASE_TAXI_OUT, PHASE_LINEUP, PHASE_TAKEOFF, PHASE_DEPARTED, &
                         WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   use sys_arrival, only: earliest_slot
   use pic_types, only: default_int
   use pic_random_dist, only: next_range
   implicit none
   private

   public :: departure_system_t
   public :: TURNAROUND_BY_WAKE

   integer(tick_k), parameter :: TURNAROUND_BY_WAKE(4) = &
                                 [25_tick_k*MINUTE, &  ! light
                                  35_tick_k*MINUTE, &  ! medium
                                  60_tick_k*MINUTE, &  ! heavy
                                  90_tick_k*MINUTE]   ! super
      !! Nominal turnaround by wake category, indexed `WAKE_*`.
      !!
      !! Bigger aircraft take longer to empty, clean, fuel and fill, which is
      !! why a Super occupies its stand for an hour and a half and why losing
      !! the Super stand to a Heavy costs so much. Milestone 1 keeps this a
      !! single number; decomposing it into deboard, clean, fuel and board is
      !! where the landside plugs in, and that is milestone 3's problem.

   integer(default_int), parameter :: TURNAROUND_JITTER_MS = 300000_default_int
      !! Spread on the turnaround, five minutes. Random determines *when*: how
      !! long a turnaround actually takes is not the player's doing, but when
      !! the stand frees changes who gets it.

   integer(tick_k), parameter :: PUSHBACK_MS = 90_tick_k*SECOND
      !! Time from cleared-to-push to clear of the stand.
   integer(tick_k), parameter :: TAKEOFF_ROLL_MS = 45_tick_k*SECOND
      !! Time from brakes-off to airborne.
   integer(tick_k), parameter :: HOLD_RETRY_MS = 2_tick_k*MINUTE
      !! How often a held departure asks again after being released.

   type, extends(system_t) :: departure_system_t
      !! Turnaround, pushback and the runway queue.
      type(stream_t) :: rng
         !! This system's own random stream.
   contains
      procedure :: handle => departure_handle
      procedure :: tick_order => departure_tick_order
      procedure :: name => departure_name
   end type departure_system_t

contains

   pure function departure_tick_order(self) result(order)
      !! Between the arrival manager and the gate system. It consumes runway
      !! state the arrival manager may have just changed, and produces stand
      !! releases the gate system then acts on.
      class(departure_system_t), intent(in) :: self
      integer(int32) :: order

      order = 5_int32
   end function departure_tick_order

   pure function departure_name(self) result(name)
      !! Short name for logs.
      class(departure_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "departure"
   end function departure_name

   subroutine departure_handle(self, w, sched, event)
      !! React to the departure events.
      class(departure_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      select case (event%kind)
      case (K_ONBLOCKS)
         call begin_turnaround(self, w, sched, event)
      case (K_TURNAROUNDCOMPLETE)
         call turnaround_complete(w, sched, event)
      case (K_PUSHBACKREQUESTED)
         call request_pushback(w, sched, event)
      case (K_PUSHBACKCOMPLETE)
         call pushback_complete(w, sched, event)
      case (K_LINEUPREQUESTED)
         call offer_slot(w, sched, event)
      case (K_LINEUPCLEARED)
         call line_up(w, sched, event)
      case (K_TAKEOFFROLLCOMPLETE)
         call airborne(w, event)
      case (K_CMD_HOLD_DEPARTURE)
         call commanded_hold(w, event, .true.)
      case (K_CMD_RELEASE_DEPARTURE)
         call commanded_hold(w, event, .false.)
         call resume_after_release(w, sched, event)
      case default
         ! Not ours.
      end select
   end subroutine departure_handle

   pure logical function known(w, aircraft)
      !! Whether a handle names an aircraft that exists.
      type(world_t), intent(in) :: w
      integer(id_k), intent(in) :: aircraft
         !! Handle to check.

      known = aircraft >= 1_id_k .and. int(aircraft, default_int) <= w%aircraft%size()
   end function known

   subroutine begin_turnaround(self, w, sched, event)
      !! Chocks in. The aircraft becomes next flight's departure.
      class(departure_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft
      integer(tick_k) :: duration

      aircraft = event%entity
      if (.not. known(w, aircraft)) return

      duration = TURNAROUND_BY_WAKE(w%aircraft%wake(aircraft)) + &
                 int(next_range(self%rng%gen, 0_default_int, TURNAROUND_JITTER_MS), tick_k)

      w%aircraft%phase(aircraft) = PHASE_TURNAROUND
      call sched%push(at=w%now + duration, kind=K_TURNAROUNDCOMPLETE, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft))
   end subroutine begin_turnaround

   subroutine turnaround_complete(w, sched, event)
      !! Ready to go. Ask for pushback.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft

      aircraft = event%entity
      if (.not. known(w, aircraft)) return

      w%aircraft%phase(aircraft) = PHASE_READY
      w%aircraft%ready_tick(aircraft) = w%now

      call sched%push(at=w%now, kind=K_PUSHBACKREQUESTED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft))
   end subroutine turnaround_complete

   subroutine request_pushback(w, sched, event)
      !! Push, unless the player is holding this one on its stand.
      !!
      !! A held departure simply does not push. It keeps its stand, which is
      !! the cost the player is choosing to pay -- the stand it is occupying is
      !! one an arrival cannot have.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft

      aircraft = event%entity
      if (.not. known(w, aircraft)) return
      if (w%aircraft%phase(aircraft) /= PHASE_READY) return
      if (w%aircraft%held(aircraft) /= 0_int32) return

      w%aircraft%phase(aircraft) = PHASE_PUSHBACK
      call sched%push(at=w%now + PUSHBACK_MS, kind=K_PUSHBACKCOMPLETE, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft))
   end subroutine request_pushback

   subroutine pushback_complete(w, sched, event)
      !! Clear of the stand. Give the gate back and head for the runway.
      !!
      !! The stand is released by event rather than by writing `gate_occupant`
      !! here: the gate system owns that array, and a departure telling it
      !! directly is how two systems end up disagreeing about who is parked
      !! where.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway

      aircraft = event%entity
      if (.not. known(w, aircraft)) return

      ! Milestone 1 is one runway. Choosing between several is the departure
      ! manager's job the moment there are several, and it is not yet.
      runway = 1_id_k
      if (w%n_runways < 1_int32) return

      w%aircraft%phase(aircraft) = PHASE_TAXI_OUT
      w%aircraft%goal(aircraft) = w%runway_threshold(runway)

      call sched%push(at=w%now, kind=K_GATERELEASED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft), &
                      payload=int(w%aircraft%gate(aircraft), int64))
   end subroutine pushback_complete

   subroutine offer_slot(w, sched, event)
      !! A departure is at the holding point. Work out when it may roll.
      !!
      !! **This is the call the separation matrix has been waiting for since
      !! milestone 0.** `earliest_slot` asks what used the runway last and how
      !! big it was: a Light behind a Super waits four minutes, a Heavy behind
      !! a Medium waits one. Which means the order departures reach this point
      !! decides how many of them get away in an hour, and that is the whole
      !! reason `hold_departure` exists.
      !!
      !! Slots are taken first-come. Reordering the queue to group wake
      !! categories would buy throughput and delete the decision.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway
      integer(tick_k) :: slot

      aircraft = event%entity
      if (.not. known(w, aircraft)) return
      if (w%aircraft%phase(aircraft) /= PHASE_LINEUP) return

      runway = 1_id_k
      if (w%n_runways < 1_int32) return

      slot = earliest_slot(w, runway, w%aircraft%wake(aircraft))

      ! Claim the slot now, so the next departure to ask is separated from this
      ! one rather than from whatever last actually rolled. Without it, a queue
      ! of five would all be offered the same instant.
      !
      ! `earliest_slot` already took the maximum against the current
      ! reservation, so this only ever moves it forward.
      w%runway_free_at(runway) = slot
      w%runway_last_wake(runway) = w%aircraft%wake(aircraft)

      call sched%push(at=slot, kind=K_LINEUPCLEARED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine offer_slot

   subroutine line_up(w, sched, event)
      !! Cleared to line up and roll.
      !!
      !! The slot was computed when the aircraft reached the threshold, against
      !! the separation the preceding movement demands. Taking it now also
      !! claims the runway for the next one.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway

      aircraft = event%entity
      if (.not. known(w, aircraft)) return

      runway = int(event%payload, id_k)
      if (runway < 1_id_k .or. runway > w%n_runways) return

      w%aircraft%phase(aircraft) = PHASE_TAKEOFF
      w%runway_last_wake(runway) = w%aircraft%wake(aircraft)
      ! `offer_slot` already reserved this runway out to at least this instant.
      ! Writing the roll-out time flat would move the reservation backwards and
      ! release the next departure early.
      w%runway_free_at(runway) = max(w%runway_free_at(runway), w%now + TAKEOFF_ROLL_MS)

      call sched%push(at=w%now + TAKEOFF_ROLL_MS, kind=K_TAKEOFFROLLCOMPLETE, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine line_up

   subroutine airborne(w, event)
      !! Wheels up. The movement is complete.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft

      aircraft = event%entity
      if (.not. known(w, aircraft)) return

      w%aircraft%phase(aircraft) = PHASE_DEPARTED
      w%aircraft%airborne_tick(aircraft) = w%now

      ! Delay is measured from ready to airborne: everything between the
      ! turnaround finishing and the wheels leaving the ground is time the
      ! airfield cost this flight, whether it was spent held on the stand, in
      ! the taxi queue, or waiting on wake separation.
      w%aircraft%delay_ms(aircraft) = w%now - w%aircraft%ready_tick(aircraft)
   end subroutine airborne

   subroutine commanded_hold(w, event, holding)
      !! Set or clear the hold flag on a departure.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event
      logical, intent(in) :: holding
         !! `.true.` to hold, `.false.` to release.

      type(command_t) :: command
      integer(tick_k) :: issued_at
      integer(id_k) :: aircraft

      call w%commands%get(event%payload, issued_at, command)
      aircraft = command%a
      if (.not. known(w, aircraft)) return

      if (holding) then
         w%aircraft%held(aircraft) = 1_int32
      else
         w%aircraft%held(aircraft) = 0_int32
      end if
   end subroutine commanded_hold

   subroutine resume_after_release(w, sched, event)
      !! A released departure asks for pushback again.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      type(command_t) :: command
      integer(tick_k) :: issued_at
      integer(id_k) :: aircraft

      call w%commands%get(event%payload, issued_at, command)
      aircraft = command%a
      if (.not. known(w, aircraft)) return
      if (w%aircraft%phase(aircraft) /= PHASE_READY) return

      call sched%push(at=w%now + HOLD_RETRY_MS, kind=K_PUSHBACKREQUESTED, entity=aircraft, &
                      generation=w%aircraft%generation(aircraft))
   end subroutine resume_after_release

end module sys_departure
