! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The arrival manager: touchdown, landing roll, vacating the runway.
module sys_arrival
   !! Sole writer of: `aircraft%phase` while an arrival is on the runway,
   !! `aircraft%touchdown_tick`, `runway_free_at` and `runway_last_wake`.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_time, only: SECOND
   use core_event, only: event_t
   use core_event_kinds, only: K_TOUCHDOWN, K_ROLLOUTCOMPLETE, K_RUNWAYEXITED
   use core_rng, only: stream_t
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_world, only: world_t, PHASE_LANDING, PHASE_ROLLOUT
   use pic_types, only: default_int
   use pic_random_dist, only: next_range
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
      case (K_TOUCHDOWN)
         call on_touchdown(self, w, sched, event)
      case (K_ROLLOUTCOMPLETE)
         call on_rollout_complete(w, sched, event)
      case default
         ! Not ours. A system is told about kinds it subscribed to, but being
         ! handed something else is not worth an error.
      end select
   end subroutine arrival_handle

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

      w%aircraft%phase(aircraft) = PHASE_LANDING
      w%aircraft%touchdown_tick(aircraft) = w%now

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

      call sched%push(at=w%now + RUNWAY_EXIT_MS, kind=K_RUNWAYEXITED, &
                      entity=aircraft, generation=w%aircraft%generation(aircraft), &
                      payload=int(runway, int64))
   end subroutine on_rollout_complete

end module sys_arrival
