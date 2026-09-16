! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for declared capacity, holding, and diversion.
module test_sys_ops_policy
   !! The airport is not open or closed, and this is where that claim is
   !! checked: every partial state -- arrivals trimmed, departures trimmed,
   !! everything at zero -- comes out of the same two integers with no special
   !! case anywhere.
   !!
   !! `the_closure_cascade` is the milestone 1 payoff in one test. Design
   !! section 6.6 describes three subsystems failing for different reasons from
   !! one weather event, and none of it is written down as a rule: departures
   !! keep their stands because they cannot push, arrivals hold because there
   !! is nowhere to park, and they divert because holding burns fuel.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_command, only: command_t
   use core_event_kinds, only: K_CMD_SET_VISIBILITY, K_CMD_CLOSE_RUNWAY, K_CMD_OPEN_RUNWAY
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_sim, only: sim_t, HOUR, MINUTE, SECOND, DEFAULT_FUEL_MS, &
                       WAKE_MEDIUM, WAKE_SUPER, PHASE_APPROACH, PHASE_DIVERTED, &
                       PHASE_HOLDING, PHASE_DEPARTED
   use sys_ops_policy, only: rate_spacing_ms, rates_for_visibility, &
                             NOMINAL_ARR_RATE, NOMINAL_DEP_RATE
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_sys_ops_policy_tests

   integer(int32), parameter :: N_GATES = 2_int32
      !! Deliberately few. Stands are what run out first in a closure.

contains

   subroutine collect_sys_ops_policy_tests(testsuite)
      !! Register the capacity tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("good_visibility_is_nominal", test_nominal), &
                  new_unittest("below_minima_is_rates_at_zero", test_below_minima), &
                  new_unittest("low_visibility_trims_both", test_low_vis), &
                  new_unittest("reduced_visibility_trims_arrivals_only", test_reduced), &
                  new_unittest("rate_becomes_a_spacing", test_spacing), &
                  new_unittest("a_closed_rate_has_no_spacing", test_closed_spacing), &
                  new_unittest("weather_reaches_the_rates", test_weather_applies), &
                  new_unittest("a_closure_makes_arrivals_hold", test_holding), &
                  new_unittest("holding_past_bingo_diverts", test_diversion), &
                  new_unittest("landing_cancels_the_bingo_timer", test_bingo_tombstoned), &
                  new_unittest("the_closure_cascade", test_cascade) &
                  ]
   end subroutine collect_sys_ops_policy_tests

   subroutine test_nominal(error)
      !! Clear weather is the nominal rate.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: arr, dep

      call rates_for_visibility(10000_int32, arr, dep)
      call check(error, arr == NOMINAL_ARR_RATE, "arrivals")
      if (allocated(error)) return
      call check(error, dep == NOMINAL_DEP_RATE, "departures")
   end subroutine test_nominal

   subroutine test_below_minima(error)
      !! Closed is the same two integers at zero, not a separate state.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: arr, dep

      call rates_for_visibility(400_int32, arr, dep)
      call check(error, arr == 0_int32, "arrivals should be zero below minima")
      if (allocated(error)) return
      call check(error, dep == 0_int32, "departures should be zero below minima")
   end subroutine test_below_minima

   subroutine test_low_vis(error)
      !! Low-visibility procedures cost arrivals more than departures.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: arr, dep

      call rates_for_visibility(900_int32, arr, dep)
      call check(error, arr == NOMINAL_ARR_RATE/2_int32, "arrivals should halve")
      if (allocated(error)) return
      call check(error, dep == (NOMINAL_DEP_RATE*2_int32)/3_int32, "departures should lose a third")
      if (allocated(error)) return
      call check(error, arr < dep, "arrivals should suffer more than departures under LVP")
   end subroutine test_low_vis

   subroutine test_reduced(error)
      !! Merely reduced visibility leaves departures alone.
      type(error_type), allocatable, intent(out) :: error

      integer(int32) :: arr, dep

      call rates_for_visibility(3000_int32, arr, dep)
      call check(error, arr == (NOMINAL_ARR_RATE*4_int32)/5_int32, "arrivals should be trimmed")
      if (allocated(error)) return
      call check(error, dep == NOMINAL_DEP_RATE, "departures should be untouched")
   end subroutine test_reduced

   subroutine test_spacing(error)
      !! A rate is a spacing, computed in integers.
      type(error_type), allocatable, intent(out) :: error

      call check(error, rate_spacing_ms(30_int32) == 120_tick_k*SECOND, "thirty an hour is two minutes")
      if (allocated(error)) return
      call check(error, rate_spacing_ms(60_int32) == 60_tick_k*SECOND, "sixty an hour is one minute")
      if (allocated(error)) return
      call check(error, rate_spacing_ms(1_int32) == HOUR, "one an hour is an hour")
   end subroutine test_spacing

   subroutine test_closed_spacing(error)
      !! Zero is closed, and says so rather than dividing by zero.
      type(error_type), allocatable, intent(out) :: error

      call check(error, rate_spacing_ms(0_int32) < 0_tick_k, "zero should report closed")
      if (allocated(error)) return
      call check(error, rate_spacing_ms(-5_int32) < 0_tick_k, "a negative rate should report closed")
   end subroutine test_closed_spacing

   subroutine build(sim, seed, n_aircraft, err)
      !! A two-stand airport with `n_aircraft` arrivals five minutes apart.
      type(sim_t), target, intent(inout) :: sim
      integer(int64), intent(in) :: seed
         !! Master seed.
      integer(int32), intent(in) :: n_aircraft
         !! Arrivals to create.
      type(error_t), intent(inout) :: err

      integer(id_k) :: aircraft
      integer(int32) :: i

      call sim%init(seed, err)
      call sim%world%reserve(int(n_aircraft, default_int), N_GATES, 1_int32, err)

      call sim%world%graph%build(4_int32, [1_id_k, 2_id_k, 2_id_k], [2_id_k, 3_id_k, 4_id_k], &
                                 [30000_int32, 40000_int32, 40000_int32], err)
      call sim%world%reservations%reserve_pool(4_int32, &
                                               64_default_int*sim%world%aircraft%capacity(), err)
      allocate (sim%world%graph%x_cm(4), sim%world%graph%y_cm(4))
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

      do i = 1_int32, n_aircraft
         call sim%world%aircraft%add(aircraft, err)
         sim%world%callsign(aircraft) = "T"
         sim%world%aircraft%wake(aircraft) = WAKE_MEDIUM
         sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
         sim%world%aircraft%node(aircraft) = 1_id_k
         sim%world%aircraft%generation(aircraft) = 1_int32
         sim%world%aircraft%fuel_ms(aircraft) = DEFAULT_FUEL_MS
         call sim%schedule_touchdown(aircraft, 1_id_k, &
                                     6_tick_k*HOUR + int(i, tick_k)*5_tick_k*MINUTE, err)
      end do
   end subroutine build

   subroutine issue(sim, command_kind, subject, err)
      !! Submit a one-argument command.
      type(sim_t), intent(inout) :: sim
      integer(int16), intent(in) :: command_kind
         !! One of the `K_CMD_*` identifiers.
      integer(id_k), intent(in) :: subject
         !! The argument.
      type(error_t), intent(inout) :: err

      type(command_t) :: command

      command%kind = command_kind
      command%a = subject
      call sim%submit(command, err)
   end subroutine issue

   subroutine test_weather_applies(error)
      !! A visibility command reaches the declared rates.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, 1_int32, err)
      call sim%run_until(5_tick_k*HOUR, err)
      call issue(sim, K_CMD_SET_VISIBILITY, 400_id_k, err)
      call sim%run_until(5_tick_k*HOUR + 1_tick_k*MINUTE, err)

      call check(error, sim%world%visibility_m == 400_int32, "visibility was not recorded")
      if (allocated(error)) return
      call check(error, sim%world%arr_rate_per_hour == 0_int32, "arrivals were not stopped")
      if (allocated(error)) return
      call check(error, sim%world%dep_rate_per_hour == 0_int32, "departures were not stopped")

      call sim%destroy()
   end subroutine test_weather_applies

   subroutine test_holding(error)
      !! An arrival refused a clearance holds rather than landing anyway.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, 1_int32, err)
      call sim%run_until(5_tick_k*HOUR, err)
      call issue(sim, K_CMD_SET_VISIBILITY, 400_id_k, err)
      ! Its slot was 06:05; give it a few circuits, but stay well inside the
      ! forty-five minutes of fuel it carries.
      call sim%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)

      call check(error, sim%world%aircraft%phase(1) == PHASE_HOLDING, "it is not holding")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) == 0_tick_k, &
                 "it landed into a closed airport")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%holds(1) > 0_int32, "no holding circuits were counted")

      call sim%destroy()
   end subroutine test_holding

   subroutine test_diversion(error)
      !! Holding past bingo fuel is a diversion, and it is permanent.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, 1_int32, err)
      call sim%run_until(5_tick_k*HOUR, err)
      call issue(sim, K_CMD_SET_VISIBILITY, 400_id_k, err)

      ! Its slot is 06:05 and it carries forty-five minutes. Run past that.
      call sim%run_until(7_tick_k*HOUR + 30_tick_k*MINUTE, err)
      call check(error, sim%world%aircraft%phase(1) == PHASE_DIVERTED, &
                 "it should have run out of holding fuel")
      if (allocated(error)) return

      ! Reopening does not bring it back.
      call issue(sim, K_CMD_SET_VISIBILITY, 9000_id_k, err)
      call sim%run_until(12_tick_k*HOUR, err)
      call check(error, sim%world%aircraft%phase(1) == PHASE_DIVERTED, &
                 "a diverted aircraft came back when the weather cleared")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%touchdown_tick(1) == 0_tick_k, &
                 "a diverted aircraft landed after all")

      call sim%destroy()
   end subroutine test_diversion

   subroutine test_bingo_tombstoned(error)
      !! An aircraft that lands does not divert later.
      !!
      !! The bingo timer is scheduled once on entering the hold and never
      !! polled, so landing has to invalidate it. That is one generation
      !! increment, and this is the check that it happens -- without it an
      !! aircraft would park, turn round, and divert from its stand.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: sim
      type(error_t) :: err

      call build(sim, 42_int64, 1_int32, err)
      call sim%run_until(5_tick_k*HOUR, err)
      call issue(sim, K_CMD_SET_VISIBILITY, 400_id_k, err)
      call sim%run_until(6_tick_k*HOUR + 20_tick_k*MINUTE, err)
      call check(error, sim%world%aircraft%phase(1) == PHASE_HOLDING, "it should be holding")
      if (allocated(error)) return

      ! Clear the weather well inside its fuel, then run long past the moment
      ! the original bingo timer would have fired.
      call issue(sim, K_CMD_SET_VISIBILITY, 9000_id_k, err)
      call sim%run_until(9_tick_k*HOUR, err)

      call check(error, sim%world%aircraft%touchdown_tick(1) > 0_tick_k, "it never landed")
      if (allocated(error)) return
      call check(error, sim%world%aircraft%phase(1) /= PHASE_DIVERTED, &
                 "an aircraft that landed diverted anyway, so the bingo timer was not tombstoned")

      call sim%destroy()
   end subroutine test_bingo_tombstoned

   subroutine test_cascade(error)
      !! Three subsystems failing for different reasons from one weather event.
      !!
      !! Two stands, six arrivals, and a closure. Stands stay occupied because
      !! departures cannot push; arrivals hold because there is nowhere to
      !! park; holding burns fuel and somebody diverts. Nothing here is a rule
      !! anybody wrote -- it falls out of capacity being two integers.
      type(error_type), allocatable, intent(out) :: error

      type(sim_t), target :: closed, clear
      type(error_t) :: err
      integer(default_int) :: i, diverted_closed, diverted_clear

      call build(clear, 42_int64, 6_int32, err)
      call clear%run_until(20_tick_k*HOUR, err)

      call build(closed, 42_int64, 6_int32, err)
      call closed%run_until(5_tick_k*HOUR, err)
      call issue(closed, K_CMD_SET_VISIBILITY, 400_id_k, err)
      call closed%run_until(9_tick_k*HOUR, err)
      call issue(closed, K_CMD_SET_VISIBILITY, 9000_id_k, err)
      call closed%run_until(20_tick_k*HOUR, err)

      diverted_clear = 0_default_int
      diverted_closed = 0_default_int
      do i = 1_default_int, 6_default_int
         if (clear%world%aircraft%phase(i) == PHASE_DIVERTED) then
            diverted_clear = diverted_clear + 1_default_int
         end if
         if (closed%world%aircraft%phase(i) == PHASE_DIVERTED) then
            diverted_closed = diverted_closed + 1_default_int
         end if
      end do

      call check(error, diverted_clear == 0_default_int, &
                 "aircraft diverted on a day with no weather at all")
      if (allocated(error)) return
      call check(error, diverted_closed > 0_default_int, &
                 "a four-hour closure diverted nobody")
      if (allocated(error)) return
      call check(error, closed%log%digest() /= clear%log%digest(), &
                 "the closure changed nothing about the run")

      call closed%destroy()
      call clear%destroy()
   end subroutine test_cascade

end module test_sys_ops_policy
