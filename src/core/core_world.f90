! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The world: every byte of canonical simulation state, in one place.
module core_world
   !! There is no `airport_state` god object holding runways and shops, and
   !! there is no fully distributed design where each system privately owns its
   !! arrays either. One struct holds all the state; systems own the behaviour
   !! and are the documented sole writers of specific fields.
   !!
   !! The payoff is that save, load and hash are operations on one object
   !! rather than a protocol negotiated across a dozen modules.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use core_aircraft, only: aircraft_soa_t
   use core_command, only: command_log_t
   use core_graph, only: taxi_graph_t
   use pic_types, only: default_int
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_VALIDATION
   implicit none
   private

   public :: world_t
   public :: CALLSIGN_LEN
   public :: PHASE_INBOUND, PHASE_APPROACH, PHASE_LANDING, PHASE_ROLLOUT
   public :: PHASE_TAXI_IN, PHASE_AT_GATE, PHASE_DIVERTED
   public :: PHASE_TURNAROUND, PHASE_READY, PHASE_PUSHBACK, PHASE_TAXI_OUT
   public :: PHASE_LINEUP, PHASE_TAKEOFF, PHASE_DEPARTED
   public :: WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   public :: phase_name, wake_letter

   integer, parameter :: CALLSIGN_LEN = 8
      !! Width of a callsign. Fixed, so the array is contiguous and a
      !! checkpoint stays one write.

   ! Phases of flight. Milestone 0 exercises the arrival half only; the
   ! departure phases arrive with the departure manager in milestone 1.
   integer(int32), parameter :: PHASE_INBOUND = 0_int32
   integer(int32), parameter :: PHASE_APPROACH = 1_int32
   integer(int32), parameter :: PHASE_LANDING = 2_int32
   integer(int32), parameter :: PHASE_ROLLOUT = 3_int32
   integer(int32), parameter :: PHASE_TAXI_IN = 4_int32
   integer(int32), parameter :: PHASE_AT_GATE = 5_int32
   integer(int32), parameter :: PHASE_DIVERTED = 6_int32
   ! The departure half. Appended rather than inserted: these numbers are in
   ! every checkpoint and every saved board.
   integer(int32), parameter :: PHASE_TURNAROUND = 7_int32
   integer(int32), parameter :: PHASE_READY = 8_int32
   integer(int32), parameter :: PHASE_PUSHBACK = 9_int32
   integer(int32), parameter :: PHASE_TAXI_OUT = 10_int32
   integer(int32), parameter :: PHASE_LINEUP = 11_int32
   integer(int32), parameter :: PHASE_TAKEOFF = 12_int32
   integer(int32), parameter :: PHASE_DEPARTED = 13_int32

   ! Wake turbulence categories. The numbering is the index into the
   ! separation matrix, so it must stay dense and ascending by size.
   integer(int32), parameter :: WAKE_LIGHT = 1_int32
   integer(int32), parameter :: WAKE_MEDIUM = 2_int32
   integer(int32), parameter :: WAKE_HEAVY = 3_int32
   integer(int32), parameter :: WAKE_SUPER = 4_int32

   type :: world_t
      !! All canonical simulation state.
      integer(tick_k) :: now = 0_tick_k
         !! Current sim time. Written only by the main loop.

      type(aircraft_soa_t) :: aircraft
         !! Aircraft state, generated from one field list.

      character(len=CALLSIGN_LEN), allocatable :: callsign(:)
         !! Callsign per aircraft, parallel to `aircraft`. Identity rather than
         !! state: it never affects timing, so it is kept out of the hashed
         !! field list and lives here instead.

      ! ---- gates ----
      integer(id_k), allocatable :: gate_occupant(:)
         !! Aircraft on each gate, or `NO_ID`. Written only by the gate system.
      integer(id_k), allocatable :: gate_node(:)
         !! Taxiway node each gate sits on.
      integer(int32), allocatable :: gate_max_wake(:)
         !! Largest wake category each gate accepts.
      character(len=CALLSIGN_LEN), allocatable :: gate_name(:)
         !! Stand label, for the ops board.
      integer(int32) :: n_gates = 0_int32
         !! Gates at the airport.

      ! ---- runways ----
      integer(id_k), allocatable :: runway_threshold(:)
         !! Landing threshold node of each runway.
      integer(id_k), allocatable :: runway_exit(:)
         !! Node an arrival vacates onto.
      integer(tick_k), allocatable :: runway_free_at(:)
         !! Earliest sim time each runway is available again.
      integer(int32), allocatable :: runway_last_wake(:)
         !! Wake category of the last movement, for separation.
      character(len=CALLSIGN_LEN), allocatable :: runway_name(:)
         !! Runway designator, for the ops board.
      integer(int32) :: n_runways = 0_int32
         !! Runways at the airport.

      ! ---- capacity and ops policy ----
      integer(int32) :: arr_rate_per_hour = 30_int32
         !! Declared arrival rate. Written only by the ops policy system.
      integer(int32) :: dep_rate_per_hour = 30_int32
         !! Declared departure rate.
      integer(int32) :: visibility_m = 10000_int32
         !! Reported visibility in metres.
      integer(int32) :: wind_kt = 0_int32
         !! Reported surface wind in knots.

      ! ---- books, integer cents throughout ----
      integer(int64) :: cash_cents = 0_int64
         !! Cash on hand. Milestone 2.
      integer(int32) :: reputation = 500_int32
         !! Reputation, 0 to 1000. Milestone 2.

      type(command_log_t) :: commands
         !! Every command the player has issued. Canonical state, and the other
         !! half of `(seed, command log)`. It lives here rather than in `sim_t`
         !! so that a handler resolving a command's payload index can reach it
         !! through the world it is already given.

      type(taxi_graph_t) :: graph
         !! Static taxiway topology.
   contains
      procedure :: reserve => world_reserve
      procedure :: free_gate_for => world_free_gate_for
      procedure :: has_stand_for => world_has_stand_for
      procedure :: destroy => world_destroy
   end type world_t

contains

   subroutine world_reserve(this, max_aircraft, n_gates, n_runways, err)
      !! Size every array once, at load.
      !!
      !! Nothing in `core/` allocates after this returns. That is not a
      !! performance rule, it is a determinism rule: an allocation that fails
      !! or succeeds depending on the host cannot then change what the
      !! simulation does.
      class(world_t), intent(inout) :: this
      integer(default_int), intent(in) :: max_aircraft
         !! Aircraft the scenario may ever have airborne or on the ground.
      integer(int32), intent(in) :: n_gates
         !! Gates at the airport.
      integer(int32), intent(in) :: n_runways
         !! Runways at the airport.
      type(error_t), intent(inout), optional :: err

      integer :: status

      if (max_aircraft < 0_default_int .or. n_gates < 0_int32 .or. n_runways < 0_int32) then
         call error_raise(err, ERROR_VALIDATION, "world_reserve: negative size")
         return
      end if

      call this%aircraft%reserve(max_aircraft, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call destroy_arrays(this)

      allocate (this%callsign(max_aircraft), &
                this%gate_occupant(n_gates), this%gate_node(n_gates), &
                this%gate_max_wake(n_gates), this%gate_name(n_gates), &
                this%runway_threshold(n_runways), this%runway_exit(n_runways), &
                this%runway_free_at(n_runways), this%runway_last_wake(n_runways), &
                this%runway_name(n_runways), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "world_reserve: allocation failed")
         return
      end if

      this%callsign = ""
      this%gate_occupant = NO_ID
      this%gate_node = NO_ID
      this%gate_max_wake = WAKE_SUPER
      this%gate_name = ""
      this%runway_threshold = NO_ID
      this%runway_exit = NO_ID
      this%runway_free_at = 0_tick_k
      this%runway_last_wake = WAKE_MEDIUM
      this%runway_name = ""
      this%n_gates = n_gates
      this%n_runways = n_runways
   end subroutine world_reserve

   pure function world_free_gate_for(this, wake) result(gate)
      !! Smallest free stand that still takes this wake category.
      !!
      !! Tightest fit, ties broken by the lowest handle. Taking simply the
      !! first free stand is one line shorter and puts the next Heavy on the
      !! only Super stand, after which the A380 behind it has nowhere to go and
      !! waits until the airport closes. That is a real failure mode and not
      !! one worth shipping at milestone 0.
      !!
      !! It is still deliberately mediocre, and meant to stay that way. It
      !! weighs no walking distance, no airline preference, no tow cost and
      !! nothing about what is inbound in twenty minutes, so a player watching
      !! the board can beat it. That is the game: the player is the optimizer,
      !! and a solver good enough to remove the decision would remove the
      !! reason to make it.
      class(world_t), intent(in) :: this
      integer(int32), intent(in) :: wake
         !! Wake category needing a stand.
      integer(id_k) :: gate

      integer(int32) :: candidate, best_wake

      gate = NO_ID
      best_wake = huge(0_int32)
      do candidate = 1_int32, this%n_gates
         if (this%gate_occupant(candidate) /= NO_ID) cycle
         if (this%gate_max_wake(candidate) < wake) cycle
         if (this%gate_max_wake(candidate) >= best_wake) cycle
         best_wake = this%gate_max_wake(candidate)
         gate = int(candidate, id_k)
      end do
   end function world_free_gate_for

   pure function world_has_stand_for(this, wake) result(exists)
      !! Whether any stand at all could take this wake category, free or not.
      !!
      !! The distinction from `free_gate_for` is the difference between an
      !! aircraft that must wait and an aircraft that must wait forever. The
      !! first is gameplay; the second is a scenario error, and conflating them
      !! turns a typo in an airport file into a queue quietly filling with
      !! retries.
      class(world_t), intent(in) :: this
      integer(int32), intent(in) :: wake
         !! Wake category needing a stand.
      logical :: exists

      integer(int32) :: candidate

      exists = .false.
      do candidate = 1_int32, this%n_gates
         if (this%gate_max_wake(candidate) < wake) cycle
         exists = .true.
         return
      end do
   end function world_has_stand_for

   subroutine destroy_arrays(this)
      !! Release everything except the aircraft container and the graph.
      class(world_t), intent(inout) :: this

      if (allocated(this%callsign)) deallocate (this%callsign)
      if (allocated(this%gate_occupant)) deallocate (this%gate_occupant)
      if (allocated(this%gate_node)) deallocate (this%gate_node)
      if (allocated(this%gate_max_wake)) deallocate (this%gate_max_wake)
      if (allocated(this%gate_name)) deallocate (this%gate_name)
      if (allocated(this%runway_threshold)) deallocate (this%runway_threshold)
      if (allocated(this%runway_exit)) deallocate (this%runway_exit)
      if (allocated(this%runway_free_at)) deallocate (this%runway_free_at)
      if (allocated(this%runway_last_wake)) deallocate (this%runway_last_wake)
      if (allocated(this%runway_name)) deallocate (this%runway_name)
      this%n_gates = 0_int32
      this%n_runways = 0_int32
   end subroutine destroy_arrays

   subroutine world_destroy(this)
      !! Release every array the world owns.
      class(world_t), intent(inout) :: this

      call this%aircraft%destroy()
      call this%commands%destroy()
      call this%graph%destroy()
      call destroy_arrays(this)
      this%now = 0_tick_k
   end subroutine world_destroy

   pure function phase_name(phase) result(name)
      !! Human-readable phase, for the ops board and event dumps.
      integer(int32), intent(in) :: phase
         !! One of the `PHASE_*` codes.
      character(len=:), allocatable :: name

      select case (phase)
      case (PHASE_INBOUND)
         name = "inbound"
      case (PHASE_APPROACH)
         name = "approach"
      case (PHASE_LANDING)
         name = "landing"
      case (PHASE_ROLLOUT)
         name = "rollout"
      case (PHASE_TAXI_IN)
         name = "taxi_in"
      case (PHASE_AT_GATE)
         name = "at_gate"
      case (PHASE_DIVERTED)
         name = "diverted"
      case (PHASE_TURNAROUND)
         name = "turnaround"
      case (PHASE_READY)
         name = "ready"
      case (PHASE_PUSHBACK)
         name = "pushback"
      case (PHASE_TAXI_OUT)
         name = "taxi_out"
      case (PHASE_LINEUP)
         name = "lineup"
      case (PHASE_TAKEOFF)
         name = "takeoff"
      case (PHASE_DEPARTED)
         name = "departed"
      case default
         name = "unknown"
      end select
   end function phase_name

   pure function wake_letter(wake) result(letter)
      !! Single-character wake category, as an ops board writes it.
      integer(int32), intent(in) :: wake
         !! One of the `WAKE_*` codes.
      character(len=1) :: letter

      select case (wake)
      case (WAKE_LIGHT)
         letter = "L"
      case (WAKE_MEDIUM)
         letter = "M"
      case (WAKE_HEAVY)
         letter = "H"
      case (WAKE_SUPER)
         letter = "S"
      case default
         letter = "?"
      end select
   end function wake_letter

end module core_world
