! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Declared capacity: what the airport is willing to accept right now.
module sys_ops_policy
   !! Sole writer of: `visibility_m`, `arr_rate_per_hour`, `dep_rate_per_hour`,
   !! and `runway_closed`.
   !!
   !! ## The airport is not open or closed
   !!
   !! Real disruption is partial. Arrivals only, departures only, one runway of
   !! two, low-visibility procedures that double the required spacing. Modelling
   !! a boolean would mean a special case for each of those, and the interesting
   !! states are all in between.
   !!
   !! So capacity is two integers, movements per hour. "Closed" is those
   !! integers at zero, and every partial case works without one extra branch.
   !! A rate becomes a minimum spacing -- thirty an hour is one every two
   !! minutes -- which the runway then honours alongside wake separation. The
   !! two are independent constraints and a movement waits for whichever binds.
   !!
   !! ## It owns four integers and nothing else
   !!
   !! This module does not know what a gate is. If a procedure here starts
   !! mentioning stands, the god object has begun.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_command, only: command_t
   use core_event, only: event_t
   use core_event_kinds, only: K_WEATHERCHANGED, K_CAPACITYCHANGED, &
                               K_CMD_SET_VISIBILITY, K_CMD_SET_ARRIVAL_RATE, &
                               K_CMD_CLOSE_RUNWAY, K_CMD_OPEN_RUNWAY
   use core_scheduler, only: scheduler_t
   use core_system, only: system_t
   use core_time, only: HOUR
   use core_world, only: world_t
   use pic_types, only: default_int
   implicit none
   private

   public :: ops_policy_system_t
   public :: NOMINAL_ARR_RATE, NOMINAL_DEP_RATE
   public :: rate_spacing_ms
   public :: rates_for_visibility

   integer(int32), parameter :: NOMINAL_ARR_RATE = 30_int32
      !! Arrivals an hour in good visibility.
   integer(int32), parameter :: NOMINAL_DEP_RATE = 30_int32
      !! Departures an hour in good visibility.

   integer(int32), parameter :: VIS_BELOW_MINIMA = 550_int32
      !! Below this, in metres, nothing moves.
   integer(int32), parameter :: VIS_LOW_PROCEDURES = 1500_int32
      !! Below this, low-visibility procedures apply and spacing doubles.
   integer(int32), parameter :: VIS_REDUCED = 5000_int32
      !! Below this, arrivals are trimmed but departures are unaffected.

   type, extends(system_t) :: ops_policy_system_t
      !! Capacity, and the weather that drives it.
   contains
      procedure :: handle => ops_handle
      procedure :: tick_order => ops_tick_order
      procedure :: name => ops_name
   end type ops_policy_system_t

contains

   pure function ops_tick_order(self) result(order)
      !! First of everything. Capacity is what the other systems read, so it
      !! has to settle before any of them acts on a tick.
      class(ops_policy_system_t), intent(in) :: self
      integer(int32) :: order

      order = -10_int32
   end function ops_tick_order

   pure function ops_name(self) result(name)
      !! Short name for logs.
      class(ops_policy_system_t), intent(in) :: self
      character(len=:), allocatable :: name

      name = "ops_policy"
   end function ops_name

   pure function rate_spacing_ms(rate_per_hour) result(spacing)
      !! Minimum gap between movements implied by a declared hourly rate.
      !!
      !! A rate of zero is closed, and reports a spacing of -1 rather than a
      !! division by zero or a very large number that would later overflow when
      !! added to a tick.
      integer(int32), intent(in) :: rate_per_hour
         !! Movements an hour; zero means closed.
      integer(tick_k) :: spacing

      if (rate_per_hour <= 0_int32) then
         spacing = -1_tick_k
         return
      end if
      ! Integer division throughout. Thirty an hour is one every 120000 ms
      ! exactly; thirty-seven an hour is one every 97297 ms, and the remainder
      ! is dropped rather than accumulated, because a spacing that drifted with
      ! rounding would make the hour's throughput depend on when it started.
      spacing = HOUR/int(rate_per_hour, tick_k)
   end function rate_spacing_ms

   pure subroutine rates_for_visibility(visibility_m, arr_rate, dep_rate)
      !! Declared rates for a reported visibility.
      !!
      !! Integer division everywhere: `(x*4)/5`, never `x*0.8`. A rate is
      !! canonical state and a real would put a rounding difference between two
      !! compilers into the arrival sequence.
      integer(int32), intent(in) :: visibility_m
         !! Reported visibility in metres.
      integer(int32), intent(out) :: arr_rate
         !! Arrivals an hour the airport will accept.
      integer(int32), intent(out) :: dep_rate
         !! Departures an hour it will release.

      arr_rate = NOMINAL_ARR_RATE
      dep_rate = NOMINAL_DEP_RATE

      if (visibility_m < VIS_BELOW_MINIMA) then
         ! Below minima. Not a special case: the same two integers, at zero.
         arr_rate = 0_int32
         dep_rate = 0_int32
      else if (visibility_m < VIS_LOW_PROCEDURES) then
         ! Low-visibility procedures. Arrivals suffer most, because the spacing
         ! that protects a missed approach is what doubles.
         arr_rate = arr_rate/2_int32
         dep_rate = (dep_rate*2_int32)/3_int32
      else if (visibility_m < VIS_REDUCED) then
         arr_rate = (arr_rate*4_int32)/5_int32
      end if
   end subroutine rates_for_visibility

   subroutine ops_handle(self, w, sched, event)
      !! React to weather and to the capacity commands.
      class(ops_policy_system_t), intent(inout) :: self
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      select case (event%kind)
      case (K_CMD_SET_VISIBILITY)
         call commanded_visibility(w, sched, event)
      case (K_WEATHERCHANGED)
         call recompute(w, sched)
      case (K_CMD_SET_ARRIVAL_RATE)
         call commanded_rate(w, sched, event)
      case (K_CMD_CLOSE_RUNWAY)
         call commanded_runway(w, event, .true.)
      case (K_CMD_OPEN_RUNWAY)
         call commanded_runway(w, event, .false.)
      case default
         ! Not ours.
      end select
   end subroutine ops_handle

   subroutine commanded_visibility(w, sched, event)
      !! The weather has been reported as something new.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      type(command_t) :: command
      integer(tick_k) :: issued_at

      call w%commands%get(event%payload, issued_at, command)
      if (command%a < 0_id_k) return

      w%visibility_m = int(command%a, int32)
      call sched%push(at=w%now, kind=K_WEATHERCHANGED, entity=NO_ID, &
                      payload=int(w%visibility_m, int64))
   end subroutine commanded_visibility

   subroutine recompute(w, sched)
      !! Work out the declared rates and announce them if they moved.
      !!
      !! Announced once, to whoever is listening. **Not broadcast to forty
      !! inbound aircraft**, each of which would then re-decide: that is where
      !! both nondeterminism and gridlock come from. Individual aircraft learn
      !! their fate at their next decision point, which is also how real
      !! operations work.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched

      integer(int32) :: arr_rate, dep_rate

      call rates_for_visibility(w%visibility_m, arr_rate, dep_rate)

      if (arr_rate == w%arr_rate_per_hour .and. dep_rate == w%dep_rate_per_hour) return

      w%arr_rate_per_hour = arr_rate
      w%dep_rate_per_hour = dep_rate

      call sched%push(at=w%now, kind=K_CAPACITYCHANGED, entity=NO_ID, &
                      payload=int(arr_rate, int64))
   end subroutine recompute

   subroutine commanded_rate(w, sched, event)
      !! The player has set an acceptance rate by hand.
      !!
      !! It sticks until the weather next changes, which then overrides it.
      !! Accepting fewer arrivals than the weather allows is a real decision --
      !! it buys stand headroom -- and the weather having the last word is what
      !! stops it from being a way to ignore the weather.
      type(world_t), intent(inout) :: w
      type(scheduler_t), intent(inout) :: sched
      type(event_t), intent(in) :: event

      type(command_t) :: command
      integer(tick_k) :: issued_at

      call w%commands%get(event%payload, issued_at, command)
      if (command%a < 0_id_k) return

      w%arr_rate_per_hour = int(command%a, int32)
      call sched%push(at=w%now, kind=K_CAPACITYCHANGED, entity=NO_ID, &
                      payload=int(w%arr_rate_per_hour, int64))
   end subroutine commanded_rate

   subroutine commanded_runway(w, event, closing)
      !! Take a runway out of use, or put it back.
      type(world_t), intent(inout) :: w
      type(event_t), intent(in) :: event
      logical, intent(in) :: closing
         !! `.true.` to close, `.false.` to open.

      type(command_t) :: command
      integer(tick_k) :: issued_at
      integer(id_k) :: runway

      call w%commands%get(event%payload, issued_at, command)
      runway = command%a
      if (runway < 1_id_k .or. runway > w%n_runways) return

      w%runway_closed(runway) = closing
   end subroutine commanded_runway

end module sys_ops_policy
