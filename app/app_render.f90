! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The ops board and the batch report.
module app_render
   !! The terminal is not a placeholder here, it is the right tool. Airports
   !! genuinely are strips and lists -- an arrival sequence, a gate occupancy
   !! table -- which is what real ops displays look like, and text renders that
   !! losslessly.
   !!
   !! Every line goes through `pic_logger`, so console verbosity and file
   !! verbosity are separable and a future interactive frame can silence the
   !! console without losing the log.
   use core_sim, only: sim_t, aircraft_view_t, gate_view_t, query_aircraft, query_gates, &
                       tick_k, id_k, NO_ID, phase_name, wake_letter, PHASE_AT_GATE
   use app_text, only: int_text, pad_left, pad_right, clock_text, duration_text
   use pic_types, only: default_int, int64
   use pic_logger, only: logger => global_logger
   implicit none
   private

   public :: render_board
   public :: render_stats

   integer(default_int), parameter :: MAX_VIEWS = 256_default_int
      !! Rows the board will draw. Beyond this the board is the wrong tool.

contains

   subroutine render_board(sim)
      !! Draw one ops board frame.
      type(sim_t), intent(in) :: sim
         !! Simulation to draw.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      type(gate_view_t) :: gates(MAX_VIEWS)
      integer(default_int) :: n_aircraft, n_gates, i
      character(len=:), allocatable :: row

      call query_aircraft(sim%world, aircraft, n_aircraft)
      call query_gates(sim%world, gates, n_gates)

      call logger%info("")
      call logger%info(" FAIRPORT            "//clock_text(sim%world%now)// &
                       "     VIS "//int_text(int(sim%world%visibility_m, int64))//"m"// &
                       "   ARR "//int_text(int(sim%world%arr_rate_per_hour, int64))//"/hr"// &
                       "   DEP "//int_text(int(sim%world%dep_rate_per_hour, int64))//"/hr")
      call logger%info("")
      call logger%info(" #  CALL      TYPE  PHASE      NODE  GATE  TD        ONBLK     TAXI")

      do i = 1_default_int, n_aircraft
         row = " "//pad_left(int_text(int(i, int64)), 2_default_int)// &
               "  "//pad_right(trim(aircraft(i)%callsign), 8_default_int)// &
               "  "//wake_letter(aircraft(i)%wake)// &
               "     "//pad_right(phase_name(aircraft(i)%phase), 9_default_int)// &
               "  "//pad_left(int_text(int(aircraft(i)%node, int64)), 4_default_int)// &
               "  "//pad_left(gate_label(gates, n_gates, aircraft(i)%gate), 4_default_int)// &
               "  "//clock_text(aircraft(i)%touchdown_tick)// &
               "  "//on_blocks_text(aircraft(i))// &
               "  "//taxi_text(aircraft(i))
         call logger%info(row)
      end do

      call logger%info("")
      row = " GATES "
      do i = 1_default_int, n_gates
         row = row//" "//trim(gates(i)%name)//"["//occupant_text(aircraft, n_aircraft, gates(i)%occupant)//"]"
      end do
      call logger%info(row)
      call logger%info("")
   end subroutine render_board

   subroutine render_stats(sim)
      !! Print the batch report.
      !!
      !! This is the mode that gets used most: "run 200 seeds, histogram the
      !! delay minutes" is how anyone finds out whether the separation rules
      !! produce interesting decisions or noise.
      type(sim_t), intent(in) :: sim
         !! Simulation to report on.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      integer(default_int) :: n_aircraft, i, parked
      integer(int64) :: total_taxi

      call query_aircraft(sim%world, aircraft, n_aircraft)

      parked = 0_default_int
      total_taxi = 0_int64
      do i = 1_default_int, n_aircraft
         if (aircraft(i)%phase /= PHASE_AT_GATE) cycle
         parked = parked + 1_default_int
         total_taxi = total_taxi + (aircraft(i)%on_blocks_tick - aircraft(i)%touchdown_tick)
      end do

      call logger%info("")
      call logger%info(" ---- stats ----")
      call logger%info("  sim time          "//clock_text(sim%world%now))
      call logger%info("  aircraft          "//int_text(int(n_aircraft, int64)))
      call logger%info("  parked            "//int_text(int(parked, int64)))
      if (parked > 0_default_int) then
         call logger%info("  mean gate-in      "//duration_text(total_taxi/int(parked, int64)))
      end if
      call logger%info("  events dispatched "//int_text(sim%log%count()))
      call logger%info("  events tombstoned "//int_text(sim%log%stale_count()))
      call logger%info("  events scheduled  "//int_text(int(sim%sched%scheduled_total(), int64)))
      call logger%info("  event log hash    "//sim%log%digest_hex())
      call logger%info("")
   end subroutine render_stats

   pure function gate_label(gates, n_gates, gate) result(text)
      !! Stand label for a gate handle, or a dash when unassigned.
      type(gate_view_t), intent(in) :: gates(:)
         !! Gate views.
      integer(default_int), intent(in) :: n_gates
         !! Views filled.
      integer(id_k), intent(in) :: gate
         !! Gate handle, or `NO_ID`.
      character(len=:), allocatable :: text

      if (gate == NO_ID .or. int(gate, default_int) > n_gates) then
         text = "--"
         return
      end if
      text = trim(gates(gate)%name)
   end function gate_label

   pure function occupant_text(aircraft, n_aircraft, occupant) result(text)
      !! Callsign on a stand, or a dash when it is free.
      type(aircraft_view_t), intent(in) :: aircraft(:)
         !! Aircraft views.
      integer(default_int), intent(in) :: n_aircraft
         !! Views filled.
      integer(id_k), intent(in) :: occupant
         !! Aircraft handle, or `NO_ID`.
      character(len=:), allocatable :: text

      if (occupant == NO_ID .or. int(occupant, default_int) > n_aircraft) then
         text = "------"
         return
      end if
      text = trim(aircraft(occupant)%callsign)
   end function occupant_text

   pure function on_blocks_text(view) result(text)
      !! On-blocks time, or dashes if the aircraft has not parked.
      type(aircraft_view_t), intent(in) :: view
         !! Aircraft to describe.
      character(len=8) :: text

      if (view%on_blocks_tick == 0_tick_k) then
         text = "--:--:--"
         return
      end if
      text = clock_text(view%on_blocks_tick)
   end function on_blocks_text

   pure function taxi_text(view) result(text)
      !! Touchdown to on-blocks, or a dash if still moving.
      type(aircraft_view_t), intent(in) :: view
         !! Aircraft to describe.
      character(len=:), allocatable :: text

      if (view%on_blocks_tick == 0_tick_k) then
         text = "-"
         return
      end if
      text = duration_text(view%on_blocks_tick - view%touchdown_tick)
   end function taxi_text

end module app_render
