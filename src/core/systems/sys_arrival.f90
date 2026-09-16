! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The arrival manager: touchdown, landing roll, vacating the runway.
module sys_arrival
   !! Sole writer of: `aircraft%phase` while an arrival is on the runway,
   !! `aircraft%touchdown_tick`, `runway_free_at` and `runway_last_wake`.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_time, only: SECOND, MINUTE
   use core_command, only: command_t
   use core_event, only: event_t
   use core_event_kinds, only: K_TOUCHDOWN, K_ROLLOUTCOMPLETE, K_RUNWAYEXITED, &
                               K_APPROACHREQUESTED, K_HOLDENTERED, K_BINGOFUEL, &
                               K_CMD_SEQUENCE_ARRIVAL
   use core_rng, only: stream_t
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t, PHASE_LANDING, PHASE_ROLLOUT, PHASE_HOLDING, &
                         PHASE_DIVERTED, PHASE_APPROACH, APRON_BUFFER
   use pic_types, only: default_int
   use pic_random_dist, only: next_range
   use sys_ops_policy, only: rate_spacing_ms
   implicit none
   private

   public :: arrival_system_t
   public :: WAKE_SEP_SEC
   public :: earliest_slot

   integer(int32), parameter :: WAKE_SEP_SEC(4, 4) = reshape([ &
                                                             !  follower:   L    M    H    S        leader
                                                             60, 60, 60, 60, &     ! Light
                                                             120, 60, 60, 60, &     ! Medium
                                                             180, 120, 90, 60, &    ! Heavy
                                                             240, 180, 120, 90 &    ! Super
                                                             ], [4, 4], order=[2, 1])
      !! Required separation in seconds, indexed `(leader, follower)`.
      !!
      !! This matrix is what makes one runway a puzzle rather than a queue.
      !! Separation depends on the *pair*, not the individual, so the order the
      !! player chooses changes total throughput, and grouping like with like
      !! buys movements. Without it, first-come-first-served is optimal and
      !! there is no game.
      !!
      !! `order=[2, 1]` lets the table be written in readable row-major while
      !! Fortran stores it column-major, so what is in the source is what is in
      !! the manual.

   integer(tick_k), parameter :: ROLLOUT_BASE_MS = 45_tick_k*SECOND
      !! Nominal landing roll, threshold to taxi speed.
   integer(default_int), parameter :: ROLLOUT_JITTER_MS = 8000_default_int
      !! Spread of the landing roll. Random determines *when*; how long a roll
      !! takes is the one thing at this milestone that is genuinely not the
      !! player's doing.
   integer(tick_k), parameter :: RUNWAY_EXIT_MS = 20_tick_k*SECOND
      !! Time from taxi speed to fully clear of the runway.

   integer(tick_k), parameter :: HOLD_CIRCUIT_MS = 4_tick_k*MINUTE
      !! One lap of the holding pattern. An aircraft refused a clearance asks
      !! again after a circuit rather than continuously, which is both what a
      !! stack does and what keeps the queue from filling with refusals.

   type, extends(system_t) :: arrival_system_t
      !! Sequencing and the landing roll.
      type(stream_t) :: rng
         !! This system's own random stream.
   contains
      procedure :: handle => arrival_handle
      procedure :: tick_order => arrival_tick_order
      procedure :: name => arrival_name
   end type arrival_system_t

contains

   pure function arrival_tick_order(self) result(order)
      !! Runs before the gate and taxi systems: the runway is the scarce
      !! resource and its state has to settle first.
      class(arrival_system_t), intent(in) :: self
      integer(int32) :: order

      order = 0_int32
   end function arrival_tick_order

   pure function arrival_name(self) result(name)
      !! Short name for logs.
      class(arrival_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "arrival"
   end function arrival_name

   pure function earliest_slot(w, runway, follower) result(slot)
      !! Earliest tick a follower of this wake category may use the runway.
      type(world_t), intent(in) :: w
      integer(id_k), intent(in) :: runway
         !! Runway to query.
      integer(int32), intent(in) :: follower
         !! Wake category of the aircraft wanting the slot.
      integer(tick_k) :: slot

      integer(int32) :: leader
      integer(tick_k) :: separation

      leader = w%runway_last_wake(runway)
      separation = int(WAKE_SEP_SEC(leader, follower), tick_k)*SECOND
      slot = max(w%now, w%runway_free_at(runway) + separation)
   end function earliest_slot

   subroutine arrival_handle(self, w, sched, event)
      !! React to the arrival events.
      class(arrival_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      select case (event%kind)
      case (K_APPROACHREQUESTED)
         call request_approach(w, sched, event)
      case (K_BINGOFUEL)
         call bingo_fuel(w, event)
      case (K_CMD_SEQUENCE_ARRIVAL)
         call commanded_sequence(w, event)
      case (K_TOUCHDOWN)
         call on_touchdown(self, w, sched, event)
      case (K_ROLLOUTCOMPLETE)
         call on_rollout_complete(w, sched, event)
      case default
         ! Not ours. A system is told about kinds it subscribed to, but being
         ! handed something else is not worth an error.
      end select
   end subroutine arrival_handle

   subroutine request_approach(w, sched, event)
      !! An arrival wants a landing clearance.
      !!
      !! This is where an arrival finds out what the weather did. It is also
      !! the second half of the closure deadlock: departures cannot push, so
      !! they keep their stands; arrivals cannot park, so they hold; holding
      !! burns fuel, so they approach bingo. Three subsystems failing for
      !! different reasons from one weather event.
      !!
      !! Reopening is not recovery either. The stack that built up during the
      !! closure still has to be drained at the capacity that has just been
      !! restored, and that drain is the gameplay.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway, cleared
      integer(tick_k) :: spacing, slot

      aircraft = event%entity
      runway = int(event%payload, id_k)
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (runway < 1_id_k .or. runway > w%n_runways) return
      if (w%aircraft%phase(aircraft) == PHASE_DIVERTED) return

      ! A stale circuit for an aircraft that has already been cleared by
      ! somebody else's request, or has already landed. Nothing to do.
      if (w%aircraft%phase(aircraft) /= PHASE_HOLDING .and. &
          w%aircraft%phase(aircraft) /= PHASE_APPROACH) return

      spacing = rate_spacing_ms(w%arr_rate_per_hour)
      if (spacing < 0_tick_k .or. w%runway_closed(runway)) then
         call enter_hold(w, sched, aircraft, runway)
         return
      end if

      ! The runway takes arrivals at the declared rate and no faster, and the
      ! wake behind whatever used it last still applies.
      slot = max(earliest_slot(w, runway, w%aircraft%wake(aircraft)), &
                 w%runway_arr_ready_at(runway))

      ! A clearance more than one circuit away is a refusal in everything but
      ! name. Say so, so the aircraft holds and burns the fuel it is really
      ! burning, rather than sitting on a promise.
      if (slot > w%now + HOLD_CIRCUIT_MS) then
         call enter_hold(w, sched, aircraft, runway)
         return
      end if

      ! The slot goes to the front of the stack, and the arrival manager hands
      ! it over directly rather than making this aircraft defer.
      !
      ! Deferring was the obvious implementation and it was wrong: the front
      ! aircraft would not ask again for a full circuit, so the slot went
      ! unused and everybody flew another lap. On a busy clear day that cost
      ! two extra diversions and more than doubled the holding.
      !
      ! Sequencing is a controller's job. Individual aircraft ask; the
      ! controller decides who goes, which is also how real operations work.
      ! Is there anywhere to put anybody? The design document is explicit that
      ! arrivals with nowhere to park hold, and holding is where a queue
      ! belongs: in the air, costing fuel, capable of ending in a diversion.
      ! On the ground it is an aircraft sitting on the runway exit for hours,
      ! which is not a busy airport but a hole in the model.
      cleared = front_of_stack(w, aircraft)
      if (cleared == NO_ID) then
         call enter_hold(w, sched, aircraft, runway)
         return
      end if

      w%runway_arr_ready_at(runway) = slot + spacing
      call sched%push(at=slot, kind=K_TOUCHDOWN, entity=cleared, &
                      generation=w%aircraft%generation(cleared), &
                      payload=int(runway, int64))

      ! Whoever asked and did not get it flies another circuit.
      if (cleared /= aircraft) call enter_hold(w, sched, aircraft, runway)
   end subroutine request_approach

   subroutine commanded_sequence(w, event)
      !! The player has put an arrival at a place in the landing order.
      !!
      !! Position 1 is next. Zero puts it back in the unsequenced majority,
      !! which is served longest-waiting-first. Sequencing is the only way to
      !! choose *who* diverts when there is not enough runway for everybody,
      !! and choosing is the whole job.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event

      type(command_t) :: command
      integer(tick_k) :: issued_at
      integer(id_k) :: aircraft

      call w%commands%get(event%payload, issued_at, command)
      aircraft = command%a
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (command%b < 0_id_k) return

      w%aircraft%arr_sequence(aircraft) = int(command%b, int32)
   end subroutine commanded_sequence

   pure function precedes(w, left, right) result(first)
      !! Whether `left` should be given a clearance before `right`.
      !!
      !! A sequenced aircraft beats an unsequenced one, a lower position beats
      !! a higher one, and among equals the one that has been holding longest
      !! goes first. That last rule is what stops the unsequenced majority
      !! starving while the player micromanages two aircraft, and the handle
      !! breaks any remaining tie so the order never depends on which event
      !! happened to fire.
      type(world_t), intent(in) :: w
      integer(id_k), intent(in) :: left
         !! Candidate.
      integer(id_k), intent(in) :: right
         !! Candidate to compare against.
      logical :: first

      integer(int32) :: left_seq, right_seq

      left_seq = w%aircraft%arr_sequence(left)
      right_seq = w%aircraft%arr_sequence(right)

      if ((left_seq > 0_int32) .neqv. (right_seq > 0_int32)) then
         first = left_seq > 0_int32
         return
      end if
      if (left_seq /= right_seq) then
         first = left_seq < right_seq
         return
      end if
      if (w%aircraft%hold_since_tick(left) /= w%aircraft%hold_since_tick(right)) then
         ! Zero means "not holding yet", which must rank last rather than first.
         if (w%aircraft%hold_since_tick(left) == 0_tick_k) then
            first = .false.
         else if (w%aircraft%hold_since_tick(right) == 0_tick_k) then
            first = .true.
         else
            first = w%aircraft%hold_since_tick(left) < w%aircraft%hold_since_tick(right)
         end if
         return
      end if
      first = left < right
   end function precedes

   pure function can_be_taken(w, aircraft) result(ok)
      !! Whether the airport has somewhere to put this particular aircraft.
      !!
      !! Per aircraft, not per request: a free Medium stand is no use to a
      !! Super, and clearing one on the strength of the other is how an A380
      !! ends up on the taxiway with nowhere to go. Checking the asker's wake
      !! and then clearing somebody else was exactly that bug.
      type(world_t), intent(in) :: w
      integer(id_k), intent(in) :: aircraft
         !! Candidate for a clearance.
      logical :: ok

      ! No stand on the field takes it at all: it can never be cleared here,
      ! however much taxiway room there is. The apron buffer is slack for an
      ! aircraft waiting for a stand that exists, not permission to land one
      ! that will never have anywhere to go.
      if (.not. w%has_stand_for(w%aircraft%wake(aircraft))) then
         ok = .false.
         return
      end if

      ok = w%free_gate_for(w%aircraft%wake(aircraft)) /= NO_ID .or. &
           w%unstanded() < APRON_BUFFER
   end function can_be_taken

   pure function front_of_stack(w, asking) result(best)
      !! The aircraft that should get the next clearance, or `NO_ID`.
      !!
      !! Considered over everyone holding plus whoever is asking, so a
      !! clearance goes to the front of the sequence rather than to whichever
      !! aircraft's circuit came round first -- but only among those the
      !! airport can actually accommodate. A Super at the front of the queue
      !! does not stop a Medium landing when the only free stand is a Medium
      !! one; it does mean the Super gets the Super stand the moment one frees.
      type(world_t), intent(in) :: w
      integer(id_k), intent(in) :: asking
         !! The aircraft whose approach request is being handled.
      integer(id_k) :: best

      integer(default_int) :: i

      best = NO_ID
      if (can_be_taken(w, asking)) best = asking

      do i = 1_default_int, w%aircraft%size()
         if (int(i, id_k) == asking) cycle
         if (w%aircraft%phase(i) /= PHASE_HOLDING) cycle
         if (.not. can_be_taken(w, int(i, id_k))) cycle
         if (best == NO_ID) then
            best = int(i, id_k)
         else if (precedes(w, int(i, id_k), best)) then
            best = int(i, id_k)
         end if
      end do
   end function front_of_stack

   subroutine enter_hold(w, sched, aircraft, runway)
      !! Refuse the clearance and put the aircraft in the stack.
      !!
      !! The bingo-fuel event is scheduled once, on first entering the hold,
      !! and never polled. One scheduled event per aircraft, and diversions
      !! then emerge in fuel order on their own rather than from a rule anybody
      !! wrote.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      integer(id_k), intent(in) :: aircraft
         !! Aircraft being refused.
      integer(id_k), intent(in) :: runway
         !! Runway it wants.

      logical :: first_time

      first_time = w%aircraft%hold_since_tick(aircraft) == 0_tick_k

      w%aircraft%phase(aircraft) = PHASE_HOLDING
      w%aircraft%holds(aircraft) = w%aircraft%holds(aircraft) + 1_int32

      if (first_time) then
         w%aircraft%hold_since_tick(aircraft) = w%now
         call sched%push(at=w%now + w%aircraft%fuel_ms(aircraft), kind=K_BINGOFUEL, &
                         entity=aircraft, generation=w%aircraft%generation(aircraft), &
                         payload=int(runway, int64))
         call sched%push(at=w%now, kind=K_HOLDENTERED, entity=aircraft, &
                         generation=w%aircraft%generation(aircraft), &
                         payload=int(runway, int64))
      end if

      call sched%push(at=w%now + HOLD_CIRCUIT_MS, kind=K_APPROACHREQUESTED, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine enter_hold

   subroutine bingo_fuel(w, event)
      !! Out of holding fuel. The aircraft goes somewhere else.
      !!
      !! A diversion is permanent and is a hard failure on the scoreboard. If
      !! the aircraft had been sequenced in time its generation would have been
      !! bumped and this event would have been tombstoned before it fired, so
      !! reaching here means nobody found it a slot.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft

      aircraft = event%entity
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (w%aircraft%phase(aircraft) /= PHASE_HOLDING) return

      w%aircraft%phase(aircraft) = PHASE_DIVERTED
   end subroutine bingo_fuel

   subroutine on_touchdown(self, w, sched, event)
      !! Wheels on. Start the landing roll and claim the runway.
      class(arrival_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway
      integer(tick_k) :: rollout_ms

      aircraft = event%entity
      runway = int(event%payload, id_k)
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (runway < 1_id_k .or. runway > w%n_runways) return

      if (w%aircraft%phase(aircraft) == PHASE_DIVERTED) return

      ! On the ground, so whatever was still pending for this aircraft -- the
      ! bingo-fuel timer above all -- is now stale. One increment invalidates
      ! all of it, however many events there are.
      w%aircraft%generation(aircraft) = w%aircraft%generation(aircraft) + 1_int32

      w%aircraft%phase(aircraft) = PHASE_LANDING
      w%aircraft%touchdown_tick(aircraft) = w%now
      if (w%aircraft%hold_since_tick(aircraft) > 0_tick_k) then
         w%aircraft%delay_ms(aircraft) = w%now - w%aircraft%hold_since_tick(aircraft)
      end if

      rollout_ms = ROLLOUT_BASE_MS + &
                   int(next_range(self%rng%gen, 0_default_int, ROLLOUT_JITTER_MS), tick_k)

      w%runway_last_wake(runway) = w%aircraft%wake(aircraft)
      ! Never backwards. A departure may already have reserved a slot further
      ! ahead, and an arrival landing in between must not hand the runway back
      ! earlier than that reservation -- doing so let a Light roll fifteen
      ! seconds behind a Heavy, which the separation matrix forbids by three
      ! minutes.
      w%runway_free_at(runway) = max(w%runway_free_at(runway), &
                                     w%now + rollout_ms + RUNWAY_EXIT_MS)

      call sched%push(at=w%now + rollout_ms, kind=K_ROLLOUTCOMPLETE, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine on_touchdown

   subroutine on_rollout_complete(w, sched, event)
      !! Down to taxi speed. Head for the runway exit.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      integer(id_k) :: aircraft, runway

      aircraft = event%entity
      runway = int(event%payload, id_k)
      if (aircraft < 1_id_k .or. int(aircraft, default_int) > w%aircraft%size()) return
      if (runway < 1_id_k .or. runway > w%n_runways) return

      w%aircraft%phase(aircraft) = PHASE_ROLLOUT
      w%aircraft%node(aircraft) = w%runway_exit(runway)
      w%aircraft%exited_tick(aircraft) = w%now + RUNWAY_EXIT_MS

      call sched%push(at=w%now + RUNWAY_EXIT_MS, kind=K_RUNWAYEXITED, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine on_rollout_complete

end module sys_arrival
