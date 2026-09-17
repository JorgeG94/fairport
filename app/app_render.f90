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
                       tick_k, id_k, NO_ID, phase_name, wake_letter, PHASE_DIVERTED
   use app_text, only: int_text, pad_left, pad_right, clock_text, duration_text
   use pic_types, only: default_int, int32, int64
   use pic_logger, only: logger => global_logger
   implicit none
   private

   public :: render_board
   public :: render_stats
   public :: board_lines
   public :: MAX_VIEWS, BOARD_WIDTH

   integer(default_int), parameter :: MAX_VIEWS = 256_default_int
      !! Rows the board will draw. Beyond this the board is the wrong tool.

   integer, parameter :: BOARD_WIDTH = 200
      !! Row width of the board buffer. Wide enough that a gate line for a
      !! large airport is not cut, and the interactive frame truncates to the
      !! real terminal width afterwards.

contains

   subroutine render_board(sim)
      !! Draw one ops board frame through the logger.
      !!
      !! Batch mode's view. The interactive terminal cannot use this -- writing
      !! through the logger in the middle of a frame is how a TUI ends up with
      !! its own log lines scrolled through it -- so both go through
      !! `board_lines` and differ only in where the lines end up.
      type(sim_t), intent(in) :: sim
         !! Simulation to draw.

      character(len=BOARD_WIDTH) :: lines(MAX_VIEWS + 8_default_int)
      integer(default_int) :: n_lines, i

      call board_lines(sim, lines, n_lines)
      do i = 1_default_int, n_lines
         call logger%info(trim(lines(i)))
      end do
   end subroutine render_board

   subroutine board_lines(sim, lines, n_lines, selected)
      !! Fill `lines` with the ops board, one row per element.
      !!
      !! Caller-owned buffer and no I/O of any kind, so the same board serves
      !! the batch log and the interactive frame.
      type(sim_t), intent(in) :: sim
         !! Simulation to draw.
      character(len=*), intent(out) :: lines(:)
         !! Buffer to fill.
      integer(default_int), intent(out) :: n_lines
         !! Rows written.
      integer(id_k), intent(in), optional :: selected
         !! Aircraft the interactive board's cursor is on. Marked with an
         !! arrow rather than reverse video, so the board reads the same in a
         !! terminal that drops attributes and in a golden file. Absent in
         !! batch mode, where there is no cursor.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      type(gate_view_t) :: gates(MAX_VIEWS)
      integer(default_int) :: n_aircraft, n_gates, i
      character(len=:), allocatable :: row

      call query_aircraft(sim%world, aircraft, n_aircraft)
      call query_gates(sim%world, gates, n_gates)

      n_lines = 0_default_int
      call emit(lines, n_lines, "")
      call emit(lines, n_lines, " FAIRPORT            "//clock_text(sim%world%now)// &
                "     VIS "//int_text(int(sim%world%visibility_m, int64))//"m"// &
                "   ARR "//int_text(int(sim%world%arr_rate_per_hour, int64))//"/hr"// &
                "   DEP "//int_text(int(sim%world%dep_rate_per_hour, int64))//"/hr")
      call emit(lines, n_lines, "")
      call emit(lines, n_lines, &
                " #  CALL      TYPE  PHASE      H  GATE  TD        ONBLK     OFF       DLY")

      do i = 1_default_int, n_aircraft
         row = cursor_mark(aircraft(i)%id, selected)// &
               pad_left(int_text(int(i, int64)), 2_default_int)// &
               "  "//pad_right(trim(aircraft(i)%callsign), 8_default_int)// &
               "  "//wake_letter(aircraft(i)%wake)// &
               "     "//pad_right(phase_name(aircraft(i)%phase), 10_default_int)// &
               " "//hold_flag(aircraft(i))// &
               "  "//pad_left(gate_label(gates, n_gates, aircraft(i)%gate), 4_default_int)// &
               "  "//clock_text(aircraft(i)%touchdown_tick)// &
               "  "//on_blocks_text(aircraft(i))// &
               "  "//clock_or_dashes(aircraft(i)%airborne_tick)// &
               "  "//pad_left(delay_text(aircraft(i)), 6_default_int)
         if (n_lines >= int(size(lines), default_int) - 3_default_int) exit
         call emit(lines, n_lines, row)
      end do

      call emit(lines, n_lines, "")
      row = " GATES "
      do i = 1_default_int, n_gates
         row = row//" "//trim(gates(i)%name)//"["//occupant_text(aircraft, n_aircraft, gates(i)%occupant)//"]"
      end do
      call emit(lines, n_lines, row)
      call emit(lines, n_lines, "")
   end subroutine board_lines

   pure function hold_flag(view) result(flag)
      !! `H` while the player is holding this aircraft, blank otherwise.
      !!
      !! One column for both kinds of hold. They are different fields owned by
      !! different systems, but to whoever is reading the board they are the
      !! same fact: this one is waiting because you said so. Holding a
      !! departure costs delay; holding an arrival costs fuel and can end in a
      !! diversion, which the report then puts against your name.
      type(aircraft_view_t), intent(in) :: view
         !! Aircraft to describe.
      character(len=1) :: flag

      flag = " "
      if (view%held /= 0_int32 .or. view%hold_ordered /= 0_int32) flag = "H"
   end function hold_flag

   pure function cursor_mark(id, selected) result(mark)
      !! The one-character gutter in front of a board row.
      integer(id_k), intent(in) :: id
         !! Aircraft on this row.
      integer(id_k), intent(in), optional :: selected
         !! Aircraft under the cursor, if there is a cursor.
      character(len=1) :: mark

      mark = " "
      if (present(selected)) then
         if (id == selected .and. id /= NO_ID) mark = ">"
      end if
   end function cursor_mark

   pure subroutine emit(lines, n_lines, text)
      !! Append one row, silently dropping anything past the buffer.
      character(len=*), intent(inout) :: lines(:)
         !! Buffer being filled.
      integer(default_int), intent(inout) :: n_lines
         !! Rows written so far.
      character(len=*), intent(in) :: text
         !! Row to append; truncated to the buffer's row width.

      if (n_lines >= int(size(lines), default_int)) return
      n_lines = n_lines + 1_default_int
      lines(n_lines) = text
   end subroutine emit

   subroutine render_stats(sim)
      !! Print the batch report.
      !!
      !! This is the mode that gets used most: "run 200 seeds, histogram the
      !! delay minutes" is how anyone finds out whether the separation rules
      !! produce interesting decisions or noise.
      type(sim_t), intent(in) :: sim
         !! Simulation to report on.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      integer(default_int) :: n_aircraft, i, parked, departed, diverted, held_total
      integer(default_int) :: diverted_on_orders
      integer(int64) :: total_taxi, total_delay, worst_delay, total_stand_wait, worst_stand_wait

      call query_aircraft(sim%world, aircraft, n_aircraft)

      parked = 0_default_int
      departed = 0_default_int
      diverted = 0_default_int
      diverted_on_orders = 0_default_int
      held_total = 0_default_int
      total_taxi = 0_int64
      total_delay = 0_int64
      worst_delay = 0_int64
      total_stand_wait = 0_int64
      worst_stand_wait = 0_int64
      do i = 1_default_int, n_aircraft
         if (aircraft(i)%on_blocks_tick > 0_tick_k) then
            parked = parked + 1_default_int
            ! Taxi time is what is left once the wait for a stand is taken
            ! out. The two are different failures and averaging them together
            ! says nothing about either.
            total_taxi = total_taxi + (aircraft(i)%on_blocks_tick - aircraft(i)%touchdown_tick) &
                         - aircraft(i)%stand_wait_ms
            total_stand_wait = total_stand_wait + aircraft(i)%stand_wait_ms
            worst_stand_wait = max(worst_stand_wait, aircraft(i)%stand_wait_ms)
         end if
         if (aircraft(i)%phase == PHASE_DIVERTED) then
            diverted = diverted + 1_default_int
            ! Still held at the moment the fuel ran out. A hold lifted in time
            ! does not count, which is the one distinction this can draw
            ! honestly without replaying the day.
            if (aircraft(i)%hold_ordered /= 0_int32) then
               diverted_on_orders = diverted_on_orders + 1_default_int
            end if
         end if
         held_total = held_total + int(aircraft(i)%holds, default_int)
         if (aircraft(i)%airborne_tick > 0_tick_k) then
            departed = departed + 1_default_int
            total_delay = total_delay + aircraft(i)%delay_ms
            worst_delay = max(worst_delay, aircraft(i)%delay_ms)
         end if
      end do

      call logger%info("")
      call logger%info(" ---- stats ----")
      call logger%info("  sim time          "//clock_text(sim%world%now))
      call logger%info("  aircraft          "//int_text(int(n_aircraft, int64)))
      call logger%info("  parked            "//int_text(int(parked, int64)))
      call logger%info("  departed          "//int_text(int(departed, int64)))
      ! Diversions are the hard failure. Delay minutes are a score you can
      ! argue about; an aircraft that went somewhere else is not.
      call logger%info("  DIVERTED          "//int_text(int(diverted, int64)))
      ! A statement of fact, not of blame: this aircraft was still held on the
      ! player's orders when its fuel ran out. It is deliberately not a
      ! counterfactual. Holding an aircraft that was going to divert anyway
      ! counts here too, because the simulation does not run the day twice to
      ! find out, and a number that quietly guessed would be worse than one
      ! that states what happened.
      if (diverted_on_orders > 0_default_int) then
         call logger%info("    held at bingo   "//int_text(int(diverted_on_orders, int64)))
      end if
      call logger%info("  holding circuits  "//int_text(int(held_total, int64)))
      if (parked > 0_default_int) then
         call logger%info("  mean taxi-in      "//duration_text(total_taxi/int(parked, int64)))
         call logger%info("  mean stand wait   "//duration_text(total_stand_wait/int(parked, int64)))
         call logger%info("  worst stand wait  "//duration_text(worst_stand_wait))
      end if
      if (departed > 0_default_int) then
         ! Total delay minutes is the milestone 1 score. Mean and worst say
         ! different things: a good mean with a terrible worst is one aircraft
         ! that never got a slot.
         call logger%info("  mean dep delay    "//duration_text(total_delay/int(departed, int64)))
         call logger%info("  worst dep delay   "//duration_text(worst_delay))
         call logger%info("  total dep delay   "//int_text(total_delay/60000_int64)//" min")
      end if
      call logger%info("  events dispatched "//int_text(sim%log%count()))
      call logger%info("  events tombstoned "//int_text(sim%log%stale_count()))
      call logger%info("  events scheduled  "//int_text(int(sim%sched%scheduled_total(), int64)))
      call logger%info("  commands issued   "//int_text(int(sim%world%commands%size(), int64)))
      call logger%info("  command log hash  "//sim%world%commands%digest_hex())
      call logger%info("  event log hash    "//sim%log%digest_hex())
      ! The seed and the command log are the session's identity; the event log
      ! hash is what that identity produced. A replay that matches the first
      ! two and not the third is a divergence in the simulation rather than in
      ! the input, which is the distinction worth being able to make.
      call logger%info("  seed              "//int_text(sim%seed))
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

      text = clock_or_dashes(view%on_blocks_tick)
   end function on_blocks_text

   pure function clock_or_dashes(tick) result(text)
      !! A clock time, or dashes for an event that has not happened.
      integer(tick_k), intent(in) :: tick
         !! Sim time, or zero for "not yet".
      character(len=8) :: text

      if (tick == 0_tick_k) then
         text = "--:--:--"
         return
      end if
      text = clock_text(tick)
   end function clock_or_dashes

   pure function delay_text(view) result(text)
      !! Ready-to-airborne delay, a hold marker, or a dash.
      type(aircraft_view_t), intent(in) :: view
         !! Aircraft to describe.
      character(len=:), allocatable :: text

      if (view%held /= 0_int32) then
         text = "HELD"
         return
      end if
      if (view%airborne_tick == 0_tick_k) then
         text = "-"
         return
      end if
      text = duration_text(view%delay_ms)
   end function delay_text

end module app_render
