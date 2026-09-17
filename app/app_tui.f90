! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The interactive ops board.
module app_tui
   !! A day, watched rather than summarised.
   !!
   !! ## What this is not
   !!
   !! It is not where the tuning happens. Batch mode runs a day in a hundredth
   !! of a second, and "run two hundred seeds and histogram the delay minutes"
   !! is how anyone finds out whether the rules produce interesting decisions.
   !! Everything about this simulation that has been tuned was tuned that way.
   !!
   !! What this is for is the thing batch cannot show: watching the stack build
   !! while the stands stay full, and having to decide *now*.
   !!
   !! ## It changes nothing about the simulation
   !!
   !! Sim time still advances only through `run_until`, commands still go
   !! through `submit` and reach the world as scheduled events, and the
   !! event-log digest of a session driven from here is the digest of the same
   !! commands in a script. The terminal is a second consumer of the query
   !! layer, exactly as a graphical client would be.
   !!
   !! ## Restoring the terminal is the hard requirement
   !!
   !! A simulator that exits leaving the shell in raw mode with no cursor is
   !! the most user-hostile failure available, and it is the one failure a
   !! player will actually hit. `pic_term` registers an `atexit` handler and
   !! signal handlers that restore the mode, so even `error stop` and Ctrl-C
   !! come back to a usable shell. This module still unwinds deliberately on
   !! every path out, because relying on the safety net for the ordinary case
   !! is how the safety net stops being tested.
   use core_sim, only: sim_t, tick_k, id_k, NO_ID, SECOND, MINUTE, &
                       aircraft_view_t, query_aircraft, phase_name, &
                       command_t, command_name, command_kind_from_name, command_arg_count, &
                       PHASE_INBOUND, PHASE_APPROACH, PHASE_HOLDING, &
                       PHASE_TURNAROUND, PHASE_READY, PHASE_PUSHBACK, &
                       PHASE_TAXI_OUT, PHASE_LINEUP
   use app_render, only: board_lines, MAX_VIEWS, BOARD_WIDTH
   use app_scenario, only: load_scenario, parse_command, session_t
   use app_text, only: int_text, clock_text
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, error_raise, ERROR_IO
   use pic_string_type, only: string_type
   use pic_tokenizer, only: tokenize
   use pic_ansi, only: frame_t, key_event_t, pending_t, decode_keys, &
                       ansi_alt_screen_enter, ansi_alt_screen_leave, &
                       ansi_hide_cursor, ansi_show_cursor, ansi_clear_screen, &
                       KEY_CHAR, KEY_CTRL_C, KEY_ESC, KEY_ENTER, KEY_BACKSPACE, &
                       KEY_UP, KEY_DOWN
   use pic_clock, only: monotonic_ms
   use pic_term, only: term_is_tty, term_size, term_enable_vt, term_raw_enter, &
                       term_raw_leave, term_read, term_write, TERM_STDIN
   implicit none
   private

   public :: run_interactive
   public :: run_interactive_scenario

   integer(int64), parameter :: FRAME_MS = 500_int64
      !! Wall-clock milliseconds between redraws. Two a second: an ops board is
      !! not an animation, and a slower frame means more of each read's timeout
      !! is spent waiting for a key rather than spinning.

   integer(default_int), parameter :: READ_TIMEOUT_MS = 60_default_int
      !! How long a read waits for a keystroke before giving the frame loop its
      !! turn back. Short enough that a key feels immediate, long enough that
      !! an idle board is not a busy loop.

   integer, parameter :: READ_BUFFER = 64
      !! Bytes taken from the terminal per read.
   integer(default_int), parameter :: MAX_KEYS = 32_default_int
      !! Key events decoded from one read.

   integer(int32), parameter :: DEFAULT_SPEED = 60_int32
      !! Sim minutes per wall-clock minute at the starting speed, so a day
      !! takes about twenty-four minutes to watch. Adjustable in flight.

   integer(tick_k), parameter :: PRE_ROLL_MS = 5_tick_k*MINUTE
      !! How much quiet time to leave before the first movement, so the board
      !! opens with a moment to read it rather than mid-landing.

   integer, parameter :: ENTRY_LEN = 64
      !! Longest command line the board accepts. The longest real command is
      !! `sequence_departure 12 3`, so this is room to spare rather than a
      !! limit anyone meets.
   integer, parameter :: MESSAGE_LEN = 128
      !! Longest reply. Parser diagnostics are the long ones.

   type :: ui_t
      !! What the board remembers between frames.
      !!
      !! None of this is simulation state. The cursor, the speed and the line
      !! being typed exist only here, are never read by `core/`, and are not in
      !! the digest -- which is the whole reason a session can be watched at
      !! one speed and replayed at another and still be the same day.
      integer(int32) :: speed = DEFAULT_SPEED
         !! Sim minutes per wall minute.
      logical :: paused = .false.
         !! Whether time is stopped. Commands still submit while paused, and
         !! land in order when it restarts.
      logical :: running = .true.
         !! Cleared to leave.
      integer(id_k) :: selected = NO_ID
         !! Aircraft the cursor is on, by handle rather than by row.
      logical :: typing = .false.
         !! Whether the command line has focus.
      character(len=ENTRY_LEN) :: entry = ""
         !! What has been typed into it.
      integer(default_int) :: entry_len = 0_default_int
         !! How much of `entry` is real.
      character(len=MESSAGE_LEN) :: message = ""
         !! What the last command did, shown until the next one.
   end type ui_t

   integer(default_int), parameter :: FALLBACK_ROWS = 24_default_int
   integer(default_int), parameter :: FALLBACK_COLS = 100_default_int
      !! Used when the terminal will not say how big it is. A made-up size is
      !! better than refusing to start, and `term_size` reports "not available"
      !! rather than inventing one itself.

contains

   subroutine run_interactive_scenario(sim, path, seed, has_seed, err, session)
      !! Load a scenario and watch it rather than running it to completion.
      type(sim_t), target, intent(inout) :: sim
         !! Simulation to drive.
      character(len=*), intent(in) :: path
         !! Scenario to load.
      integer(int64), intent(in) :: seed
         !! Seed from the command line.
      logical, intent(in) :: has_seed
         !! Whether the command line supplied one.
      type(error_t), intent(inout), optional :: err
      type(session_t), intent(out), optional :: session
         !! The world half of the script, so the day just played can be saved.

      integer(tick_k) :: horizon

      call load_scenario(sim, path, seed, has_seed, horizon, err, session)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call run_interactive(sim, horizon, err)
   end subroutine run_interactive_scenario

   subroutine run_interactive(sim, horizon, err)
      !! Watch a simulation run, at a speed the player controls.
      type(sim_t), intent(inout) :: sim
         !! Simulation to drive. Already loaded and seeded.
      integer(tick_k), intent(in) :: horizon
         !! Sim time to stop at.
      type(error_t), intent(inout), optional :: err

      type(frame_t) :: frame
      type(pending_t) :: pending
      type(key_event_t) :: keys(MAX_KEYS)
      character(len=READ_BUFFER) :: bytes
      character(len=BOARD_WIDTH) :: lines(MAX_VIEWS + 8)
      ! One `error_t` per call, never a shared one. `error_t` accumulates: a
      ! failure left in it is still there at the next check, so reusing a
      ! single variable made an unavailable terminal size look like a failure
      ! to enter raw mode, and the board refused to start at all.
      type(error_t) :: size_fault, raw_fault, read_fault
      integer(default_int) :: rows, cols, n_keys, n_read, n_lines
      integer(int64) :: last_frame_ms, now_ms
      type(ui_t) :: ui
      logical :: at_eof

      if (.not. term_is_tty(TERM_STDIN)) then
         ! Piped input is not a mistake worth guessing about: without a
         ! terminal there is nothing to put a board on and no keys to read.
         call error_raise(err, ERROR_IO, &
                          "interactive mode needs a terminal; use a scenario file for batch runs")
         return
      end if

      ! Windows needs this to interpret escape sequences at all; elsewhere it
      ! is a no-op. Its failure is not fatal -- a terminal that will not enable
      ! virtual terminal processing will simply show the escapes.
      call term_enable_vt(raw_fault)

      call term_size(rows, cols, size_fault)
      if (size_fault%has_error() .or. rows <= 0_default_int) then
         ! `term_size` reports "not available" rather than inventing a size,
         ! which is the right call for a library and leaves the guess here.
         rows = FALLBACK_ROWS
         cols = FALLBACK_COLS
      end if

      call term_raw_enter(raw_fault)
      if (raw_fault%has_error()) then
         if (present(err)) err = raw_fault
         return
      end if

      call term_write(ansi_alt_screen_enter()//ansi_hide_cursor()//ansi_clear_screen())
      call frame%resize(rows, cols)

      ! Start the clock near the traffic. A scenario day begins at midnight
      ! and the first aircraft is often hours away, which at any watchable
      ! speed is minutes of staring at an empty board. Jumping to just before
      ! the first scheduled event costs nothing -- there is by definition
      ! nothing to dispatch in between -- and it is the difference between the
      ! board opening on an airport and opening on a clock.
      if (.not. sim%sched%is_empty()) then
         call sim%run_until(max(0_tick_k, sim%sched%next_tick() - PRE_ROLL_MS), err)
         if (present(err)) then
            if (err%has_error()) then
               call term_raw_leave()
               return
            end if
         end if
      end if

      ! Start the cursor on the first aircraft, so the shortcut keys mean
      ! something before the player has pressed an arrow.
      call move_selection(sim, ui, 0_default_int)
      last_frame_ms = monotonic_ms()

      do while (ui%running)
         ! --- input ------------------------------------------------------
         call term_read(bytes, n_read, READ_TIMEOUT_MS, read_fault, at_eof)
         if (at_eof) exit
         if (n_read > 0_default_int) then
            call decode_keys(pending, bytes(1:n_read), keys, n_keys)
            call apply_keys(sim, keys, n_keys, ui)
         end if

         ! --- time -------------------------------------------------------
         now_ms = monotonic_ms()
         if (now_ms - last_frame_ms >= FRAME_MS) then
            if (.not. ui%paused) then
               ! Sim time advances by the wall time that elapsed, scaled. It is
               ! still `run_until` and still the same event queue: the speed
               ! control changes only how much sim time one frame is worth,
               ! never how the simulation gets there.
               call sim%run_until(min(horizon, sim%world%now + &
                                      (now_ms - last_frame_ms)*int(ui%speed, tick_k)), err)
               if (present(err)) then
                  if (err%has_error()) exit
               end if
            end if
            last_frame_ms = now_ms

            call board_lines(sim, lines, n_lines, ui%selected)
            call paint(frame, lines, n_lines, rows, sim, ui)
            call term_write(frame%render())

            if (sim%world%now >= horizon) ui%running = .false.
         end if
      end do

      call term_write(ansi_show_cursor()//ansi_alt_screen_leave())
      call term_raw_leave()
   end subroutine run_interactive

   subroutine apply_keys(sim, keys, n_keys, ui)
      !! Act on whatever the player pressed.
      !!
      !! Every key that changes the simulation does so by building a
      !! `command_t` and handing it to `sim%submit`. There is no other path,
      !! which is what makes a played session a script: the log the board
      !! writes is the log a scenario file would have produced.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue commands against.
      type(key_event_t), intent(in) :: keys(:)
         !! Decoded events.
      integer(default_int), intent(in) :: n_keys
         !! Events to read.
      type(ui_t), intent(inout) :: ui
         !! Board state, which the keys move.

      integer(default_int) :: i

      do i = 1_default_int, n_keys
         if (ui%typing) then
            call typing_key(sim, keys(i), ui)
         else
            call board_key(sim, keys(i), ui)
         end if
      end do
   end subroutine apply_keys

   subroutine board_key(sim, key, ui)
      !! One key pressed while the board has focus.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue commands against.
      type(key_event_t), intent(in) :: key
         !! The key.
      type(ui_t), intent(inout) :: ui
         !! Board state.

      select case (key%code)
      case (KEY_CTRL_C, KEY_ESC)
         ui%running = .false.
      case (KEY_UP)
         call move_selection(sim, ui, -1_default_int)
      case (KEY_DOWN)
         call move_selection(sim, ui, 1_default_int)
      case (KEY_CHAR)
         select case (achar(key%char_code))
         case ("q", "Q")
            ui%running = .false.
         case (" ")
            ui%paused = .not. ui%paused
         case ("+", "=")
            ui%speed = min(ui%speed*2_int32, 3600_int32)
         case ("-", "_")
            ui%speed = max(ui%speed/2_int32, 1_int32)
         case ("k")
            call move_selection(sim, ui, -1_default_int)
         case ("j")
            call move_selection(sim, ui, 1_default_int)
         case ("h")
            call issue(sim, ui, "hold_departure", 0_id_k)
         case ("r")
            call issue(sim, ui, "release_departure", 0_id_k)
         case ("f")
            call sequence_to_front(sim, ui)
         case (":")
            ui%typing = .true.
            ui%entry = ""
            ui%entry_len = 0_default_int
            ui%message = ""
         case default
            ! Any other character is not a key this board answers to. Silently
            ! ignored rather than beeped at: a stray letter is not a mistake
            ! worth telling somebody about.
         end select
      case default
         ! Home, Delete and the rest decode correctly and mean nothing here.
      end select
   end subroutine board_key

   subroutine typing_key(sim, key, ui)
      !! One key pressed into the command line.
      !!
      !! The line is the whole command vocabulary rather than the three keys
      !! that have shortcuts, and it is the same text a scenario file holds, so
      !! there is nothing to learn twice and nothing for the two forms to
      !! disagree about.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue the command against.
      type(key_event_t), intent(in) :: key
         !! The key.
      type(ui_t), intent(inout) :: ui
         !! Board state.

      select case (key%code)
      case (KEY_ESC, KEY_CTRL_C)
         ui%typing = .false.
         ui%entry_len = 0_default_int
         ui%message = ""
      case (KEY_ENTER)
         ui%typing = .false.
         if (ui%entry_len > 0_default_int) call submit_text(sim, ui, ui%entry(1:ui%entry_len))
         ui%entry_len = 0_default_int
      case (KEY_BACKSPACE)
         if (ui%entry_len > 0_default_int) ui%entry_len = ui%entry_len - 1_default_int
      case (KEY_CHAR)
         ! Printable ASCII only. A multi-byte character arrives as its
         ! individual bytes and no command name contains one, so refusing them
         ! here keeps the buffer a string of characters rather than of bytes.
         if (key%char_code >= 32_int32 .and. key%char_code < 127_int32 .and. &
             ui%entry_len < int(ENTRY_LEN, default_int)) then
            ui%entry_len = ui%entry_len + 1_default_int
            ui%entry(ui%entry_len:ui%entry_len) = achar(key%char_code)
         end if
      case default
         ! Arrows in the command line do nothing; there is no cursor to move.
      end select
   end subroutine typing_key

   subroutine submit_text(sim, ui, text)
      !! Parse a typed line and issue it.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue the command against.
      type(ui_t), intent(inout) :: ui
         !! Board state, for the reply.
      character(len=*), intent(in) :: text
         !! What the player typed, without the leading colon.

      type(string_type), allocatable :: tokens(:)
      type(command_t) :: command
      type(error_t) :: parse_err

      tokens = tokenize(trim(text))
      if (size(tokens) == 0) return

      ! Line zero: this did not come from a file, so the diagnostic must not
      ! claim a line number.
      call parse_command(tokens, 1_default_int, 0_default_int, command, parse_err)
      if (parse_err%has_error()) then
         ui%message = parse_err%get_message()
         return
      end if

      call send(sim, ui, command)
   end subroutine submit_text

   subroutine issue(sim, ui, name, b)
      !! Issue a one-key command against the selected aircraft.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue the command against.
      type(ui_t), intent(inout) :: ui
         !! Board state, for the selection and the reply.
      character(len=*), intent(in) :: name
         !! Command name, looked up rather than hard-coded, so the shortcut
         !! cannot drift from the identifier in `commands.fypp`.
      integer(id_k), intent(in) :: b
         !! Second argument, or zero.

      type(command_t) :: command

      if (ui%selected == NO_ID) then
         ui%message = "nothing selected -- use the arrow keys"
         return
      end if

      command%kind = command_kind_from_name(name)
      command%a = ui%selected
      command%b = b
      call send(sim, ui, command)
   end subroutine issue

   subroutine sequence_to_front(sim, ui)
      !! Put the selected aircraft at the head of whichever queue it is in.
      !!
      !! Which queue that is follows from its phase, so the player presses one
      !! key and means the obvious thing. An aircraft in neither queue is told
      !! so rather than silently given a command that cannot apply.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue the command against.
      type(ui_t), intent(inout) :: ui
         !! Board state.

      type(aircraft_view_t) :: view
      logical :: found

      call selected_view(sim, ui%selected, view, found)
      if (.not. found) then
         ui%message = "nothing selected -- use the arrow keys"
         return
      end if

      select case (view%phase)
      case (PHASE_INBOUND, PHASE_APPROACH, PHASE_HOLDING)
         call issue(sim, ui, "sequence_arrival", 1_id_k)
      case (PHASE_TURNAROUND, PHASE_READY, PHASE_PUSHBACK, PHASE_TAXI_OUT, PHASE_LINEUP)
         call issue(sim, ui, "sequence_departure", 1_id_k)
      case default
         ui%message = trim(view%callsign)//" is not in a queue"
      end select
   end subroutine sequence_to_front

   subroutine send(sim, ui, command)
      !! Hand a built command to the simulation and say what happened.
      type(sim_t), intent(inout) :: sim
         !! Simulation to issue the command against.
      type(ui_t), intent(inout) :: ui
         !! Board state, for the reply.
      type(command_t), intent(in) :: command
         !! Command to issue.

      type(error_t) :: submit_err
      character(len=:), allocatable :: echo

      call sim%submit(command, submit_err)
      if (submit_err%has_error()) then
         ui%message = submit_err%get_message()
         return
      end if

      echo = trim(command_name(command%kind))//" "//int_text(int(command%a, int64))
      if (command_arg_count(command%kind) >= 2_int32) then
         echo = echo//" "//int_text(int(command%b, int64))
      end if
      ui%message = clock_text(sim%world%now)//"  "//echo
   end subroutine send

   subroutine move_selection(sim, ui, step)
      !! Move the cursor one row through the board's own ordering.
      !!
      !! The cursor remembers a handle rather than a row, because the board is
      !! a view and its ordering is not a promise. A handle that has left the
      !! board puts the cursor back at the top rather than somewhere arbitrary.
      type(sim_t), intent(in) :: sim
         !! Simulation to read.
      type(ui_t), intent(inout) :: ui
         !! Board state.
      integer(default_int), intent(in) :: step
         !! Rows to move, positive down.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      integer(default_int) :: n_aircraft, i, at

      call query_aircraft(sim%world, aircraft, n_aircraft)
      if (n_aircraft == 0_default_int) then
         ui%selected = NO_ID
         return
      end if

      at = 0_default_int
      do i = 1_default_int, n_aircraft
         if (aircraft(i)%id == ui%selected) then
            at = i
            exit
         end if
      end do

      if (at == 0_default_int) then
         at = 1_default_int
      else
         at = min(max(at + step, 1_default_int), n_aircraft)
      end if
      ui%selected = aircraft(at)%id
   end subroutine move_selection

   subroutine selected_view(sim, id, view, found)
      !! Look the selected aircraft up in the query layer.
      type(sim_t), intent(in) :: sim
         !! Simulation to read.
      integer(id_k), intent(in) :: id
         !! Handle to find.
      type(aircraft_view_t), intent(out) :: view
         !! What was found.
      logical, intent(out) :: found
         !! Whether it was.

      type(aircraft_view_t) :: aircraft(MAX_VIEWS)
      integer(default_int) :: n_aircraft, i

      found = .false.
      if (id == NO_ID) return

      call query_aircraft(sim%world, aircraft, n_aircraft)
      do i = 1_default_int, n_aircraft
         if (aircraft(i)%id == id) then
            view = aircraft(i)
            found = .true.
            return
         end if
      end do
   end subroutine selected_view

   subroutine paint(frame, lines, n_lines, rows, sim, ui)
      !! Put the board and the two bottom lines into the frame.
      type(frame_t), intent(inout) :: frame
         !! Frame to fill.
      character(len=*), intent(in) :: lines(:)
         !! Board rows from `board_lines`.
      integer(default_int), intent(in) :: n_lines
         !! Rows filled.
      integer(default_int), intent(in) :: rows
         !! Terminal height.
      type(sim_t), intent(in) :: sim
         !! Simulation, for the status line.
      type(ui_t), intent(in) :: ui
         !! Board state.

      integer(default_int) :: row, last_board_row

      ! Two rows are kept at the bottom: the reply line and the status line.
      last_board_row = min(n_lines, rows - 2_default_int)

      do row = 1_default_int, last_board_row
         call frame%set_line(row, trim(lines(row)))
      end do
      do row = last_board_row + 1_default_int, rows - 2_default_int
         call frame%set_line(row, "")
      end do

      call frame%set_line(rows - 1_default_int, reply_line(sim, ui))
      call frame%set_line(rows, status_line(sim, ui))
   end subroutine paint

   function reply_line(sim, ui) result(text)
      !! The line above the status line: what is being typed, or what the last
      !! command did, or what the cursor is on.
      type(sim_t), intent(in) :: sim
         !! Simulation to read.
      type(ui_t), intent(in) :: ui
         !! Board state.
      character(len=:), allocatable :: text

      type(aircraft_view_t) :: view
      logical :: found

      if (ui%typing) then
         ! A block for a cursor: the real one is hidden, and a terminal that
         ! drops attributes still shows where the next character lands.
         text = " :"//ui%entry(1:ui%entry_len)//"_"
      else if (len_trim(ui%message) > 0) then
         text = " "//trim(ui%message)
      else
         call selected_view(sim, ui%selected, view, found)
         if (found) then
            text = " SEL "//trim(view%callsign)//"  "//trim(phase_name(view%phase))
         else
            text = ""
         end if
      end if
   end function reply_line

   function status_line(sim, ui) result(text)
      !! The bottom line: what time it is and what the keys do.
      type(sim_t), intent(in) :: sim
         !! Simulation to report on.
      type(ui_t), intent(in) :: ui
         !! Board state.
      character(len=:), allocatable :: text

      character(len=:), allocatable :: state

      if (ui%typing) then
         text = " "//clock_text(sim%world%now)// &
                "   [enter] issue  [esc] cancel"
         return
      end if

      if (ui%paused) then
         state = "PAUSED"
      else
         state = "x"//int_text(int(ui%speed, int64))
      end if

      text = " "//clock_text(sim%world%now)//"  "//state// &
             "   [space] pause  [+/-] speed  [arrows] select"// &
             "  [h]old [r]elease [f]ront  [:] command  [q] quit"// &
             "   events "//int_text(sim%log%count())
   end function status_line

end module app_tui
