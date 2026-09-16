! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Loading a day's traffic from TOML.
module app_schedule
   !! A schedule is where the airport stops being a demonstration and starts
   !! being a problem. One aircraft lands and parks; eighty of them contend.
   !!
   !! Two ways to write one, and both are needed:
   !!
   !! - `[[arrival]]` pins an exact movement. A regression test wants the same
   !!   four aircraft at the same four times every run, and nothing derived.
   !! - `[[bank]]` states volume and shape -- twenty-four arrivals between
   !!   06:00 and 08:30, mostly medium -- and the loader draws the individual
   !!   movements from the schedule's own random stream. Writing eighty
   !!   movements by hand is how a schedule stops being edited.
   !!
   !! A bank is still reproducible. It draws from `pic_random_dist`'s integer
   !! tier through a stream derived from the master seed, so the same seed
   !! expands to the same eighty movements on every compiler. What a bank gives
   !! up is being able to read the timetable off the page, which is why the
   !! pinned form exists alongside it.
   use core_sim, only: sim_t, tick_k, id_k, HOUR, MINUTE, SECOND, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER, &
                       PHASE_APPROACH, CALLSIGN_LEN, STREAM_SCHEDULE
   use app_text, only: int_text
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, error_raise, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION
   use pic_rng, only: splitmix64_t, stream_for
   use pic_random_dist, only: next_range
   use pic_sorting, only: sort
   use pic_tokenizer, only: parse_int
   use tomlf, only: toml_table, toml_array, toml_error, toml_load, get_value, len
   implicit none
   private

   public :: load_schedule
   public :: add_arrival
   public :: parse_clock_text
   public :: wake_from_letter

   integer(int32), parameter :: PAX_BY_WAKE(4) = [60_int32, 180_int32, 300_int32, 500_int32]
      !! Default passenger load by wake category, indexed `WAKE_*`. A bank
      !! states how many aircraft and of what size; how full they are is not
      !! the interesting variable at this milestone.

contains

   subroutine load_schedule(sim, path, err)
      !! Read a schedule file and create every movement it declares.
      type(sim_t), intent(inout) :: sim
         !! Simulation to populate. Its airport must already be loaded.
      character(len=*), intent(in) :: path
         !! Path to the schedule TOML.
      type(error_t), intent(inout), optional :: err

      type(toml_table), allocatable :: table
      type(toml_error), allocatable :: parse_fault
      type(toml_array), pointer :: entries
      ! Plain default `integer` at the toml-f boundary. Its `pos` arguments are
      ! default kind, which stops matching `default_int` under
      ! PIC_DEFAULT_INT8=ON -- so the conversion happens here, once, rather
      ! than the build breaking in the other integer width.
      integer :: i

      if (sim%world%n_runways <= 0_int32) then
         call error_raise(err, ERROR_VALIDATION, &
                          "load_schedule: load an airport before a schedule")
         return
      end if

      call toml_load(table, path, error=parse_fault)
      if (allocated(parse_fault)) then
         call error_raise(err, ERROR_PARSE, "load_schedule: "//parse_fault%message)
         return
      end if
      if (.not. allocated(table)) then
         call error_raise(err, ERROR_IO, "load_schedule: cannot read "//trim(path))
         return
      end if

      ! Pinned movements first, so their handles are low and stable whatever a
      ! bank does afterwards. A scenario that names aircraft 1 in a command
      ! should keep meaning the same aircraft when a bank is resized.
      nullify (entries)
      call get_value(table, "arrival", entries)
      if (associated(entries)) then
         do i = 1, len(entries)
            call read_arrival(sim, entries, i, err)
            if (present(err)) then
               if (err%has_error()) return
            end if
         end do
      end if

      nullify (entries)
      call get_value(table, "bank", entries)
      if (associated(entries)) then
         do i = 1, len(entries)
            call expand_bank(sim, entries, i, err)
            if (present(err)) then
               if (err%has_error()) return
            end if
         end do
      end if
   end subroutine load_schedule

   subroutine read_arrival(sim, entries, index, err)
      !! One `[[arrival]]` table: an exact movement.
      type(sim_t), intent(inout) :: sim
      type(toml_array), pointer, intent(in) :: entries
         !! The array of arrival tables.
      integer, intent(in) :: index
         !! Which element to read; default integer kind, as toml-f wants.
      type(error_t), intent(inout), optional :: err

      type(toml_table), pointer :: entry
      character(len=:), allocatable :: callsign, wake_letter, at_text
      integer(int32) :: pax, runway, wake
      integer(tick_k) :: at

      nullify (entry)
      call get_value(entries, index, entry)
      if (.not. associated(entry)) then
         call error_raise(err, ERROR_PARSE, "load_schedule: [[arrival]] "// &
                          int_text(int(index, int64))//" is not a table")
         return
      end if

      call get_value(entry, "callsign", callsign, "")
      call get_value(entry, "wake", wake_letter, "M")
      call get_value(entry, "at", at_text, "")
      call get_value(entry, "runway", runway, 1_int32)
      call get_value(entry, "pax", pax, -1_int32)

      if (len_trim(callsign) == 0 .or. len_trim(at_text) == 0) then
         call error_raise(err, ERROR_PARSE, "load_schedule: [[arrival]] "// &
                          int_text(int(index, int64))//" needs a callsign and a time")
         return
      end if

      wake = wake_from_letter(wake_letter)
      if (wake == 0_int32) then
         call error_raise(err, ERROR_PARSE, "load_schedule: unknown wake category '"// &
                          wake_letter//"' in [[arrival]] "//int_text(int(index, int64)))
         return
      end if
      if (pax < 0_int32) pax = PAX_BY_WAKE(wake)

      call parse_clock_text(at_text, at, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call add_arrival(sim, callsign, wake, pax, at, int(runway, id_k), err)
   end subroutine read_arrival

   subroutine expand_bank(sim, entries, index, err)
      !! One `[[bank]]` table: volume and shape, expanded by the RNG.
      type(sim_t), intent(inout) :: sim
      type(toml_array), pointer, intent(in) :: entries
         !! The array of bank tables.
      integer, intent(in) :: index
         !! Which element to read; default integer kind, as toml-f wants.
      type(error_t), intent(inout), optional :: err

      type(toml_table), pointer :: entry
      type(toml_array), pointer :: mix
      character(len=:), allocatable :: kind, prefix, from_text, to_text
      integer(int32) :: count, runway, weights(4), i, wake, total
      integer :: slot
      integer(tick_k) :: from_tick, to_tick
      integer(int64), allocatable :: at(:)
      type(splitmix64_t) :: generator

      nullify (entry)
      call get_value(entries, index, entry)
      if (.not. associated(entry)) then
         call error_raise(err, ERROR_PARSE, "load_schedule: [[bank]] "// &
                          int_text(int(index, int64))//" is not a table")
         return
      end if

      call get_value(entry, "kind", kind, "arrival")
      call get_value(entry, "prefix", prefix, "BNK")
      call get_value(entry, "from", from_text, "")
      call get_value(entry, "to", to_text, "")
      call get_value(entry, "count", count, 0_int32)
      call get_value(entry, "runway", runway, 1_int32)

      if (kind /= "arrival") then
         ! Departures arrive with the departure manager. Naming one here is a
         ! scenario error rather than a silent no-op, so a schedule written
         ! ahead of the code fails loudly instead of quietly running light.
         call error_raise(err, ERROR_VALIDATION, "load_schedule: bank kind '"//kind// &
                          "' is not supported yet; only 'arrival'")
         return
      end if
      if (count <= 0_int32) return

      call parse_clock_text(from_text, from_tick, err)
      call parse_clock_text(to_text, to_tick, err)
      if (present(err)) then
         if (err%has_error()) return
      end if
      if (to_tick <= from_tick) then
         call error_raise(err, ERROR_VALIDATION, "load_schedule: [[bank]] "// &
                          int_text(int(index, int64))//" ends before it starts")
         return
      end if

      weights = [0_int32, 1_int32, 0_int32, 0_int32]
      nullify (mix)
      call get_value(entry, "wake_mix", mix)
      if (associated(mix)) then
         if (len(mix) == 4) then
            do slot = 1, 4
               call get_value(mix, slot, weights(slot))
            end do
         end if
      end if
      total = sum(weights)
      if (total <= 0_int32) then
         call error_raise(err, ERROR_VALIDATION, "load_schedule: [[bank]] "// &
                          int_text(int(index, int64))//" has an empty wake mix")
         return
      end if

      ! The schedule gets its own stream, so adding a bank cannot shift the
      ! arrival manager's draws and rewrite an unrelated scenario.
      call stream_for(sim%seed + int(index, int64), STREAM_SCHEDULE, generator)

      allocate (at(count))
      do i = 1_int32, count
         at(i) = int(next_range(generator, int(from_tick, default_int), &
                                int(to_tick, default_int)), int64)
      end do

      ! Sorted, so the bank is a timetable rather than the order the draws came
      ! out in. Wake categories are drawn afterwards, against the sorted times,
      ! so a movement's size is tied to its slot and not to its draw.
      call sort(at)

      do i = 1_int32, count
         wake = pick_wake(generator, weights, total)
         call add_arrival(sim, prefix//zero_padded(i), wake, PAX_BY_WAKE(wake), &
                          int(at(i), tick_k), int(runway, id_k), err)
         if (present(err)) then
            if (err%has_error()) return
         end if
      end do
   end subroutine expand_bank

   function pick_wake(generator, weights, total) result(wake)
      !! Draw a wake category against integer weights.
      !!
      !! Integer arithmetic throughout: the draw reaches simulation state, so
      !! it has to mean the same thing on every compiler.
      type(splitmix64_t), intent(inout) :: generator
         !! The schedule's stream.
      integer(int32), intent(in) :: weights(4)
         !! Relative weights for light, medium, heavy and super.
      integer(int32), intent(in) :: total
         !! Sum of `weights`, which the caller has checked is positive.
      integer(int32) :: wake

      integer(int32) :: draw, running, i

      draw = int(next_range(generator, 1_default_int, int(total, default_int)), int32)
      running = 0_int32
      wake = WAKE_MEDIUM
      do i = 1_int32, 4_int32
         running = running + weights(i)
         if (draw <= running) then
            wake = i
            return
         end if
      end do
   end function pick_wake

   subroutine add_arrival(sim, callsign, wake, pax, at, runway, err)
      !! Create one inbound aircraft and schedule its touchdown.
      !!
      !! The single path into the aircraft container. The scenario file's
      !! `arrival` directive and the schedule loader both come through here, so
      !! a movement means the same thing however it was written.
      type(sim_t), intent(inout) :: sim
      character(len=*), intent(in) :: callsign
         !! Callsign; truncated to `CALLSIGN_LEN`.
      integer(int32), intent(in) :: wake
         !! One of the `WAKE_*` codes.
      integer(int32), intent(in) :: pax
         !! Passengers on board.
      integer(tick_k), intent(in) :: at
         !! Sim time of touchdown.
      integer(id_k), intent(in) :: runway
         !! Runway it lands on.
      type(error_t), intent(inout), optional :: err

      integer(id_k) :: aircraft

      if (runway < 1_id_k .or. runway > sim%world%n_runways) then
         call error_raise(err, ERROR_VALIDATION, "add_arrival: "//trim(callsign)// &
                          " names a runway this airport does not have")
         return
      end if
      if (.not. sim%world%has_stand_for(wake)) then
         call error_raise(err, ERROR_VALIDATION, "add_arrival: no stand at this airport takes "// &
                          trim(callsign))
         return
      end if

      call sim%world%aircraft%add(aircraft, err)
      if (aircraft == 0_id_k) return

      sim%world%callsign(aircraft) = callsign
      sim%world%aircraft%wake(aircraft) = wake
      sim%world%aircraft%pax(aircraft) = pax
      sim%world%aircraft%phase(aircraft) = PHASE_APPROACH
      sim%world%aircraft%node(aircraft) = sim%world%runway_threshold(runway)
      sim%world%aircraft%generation(aircraft) = 1_int32

      call sim%schedule_touchdown(aircraft, runway, at, err)
   end subroutine add_arrival

   pure function wake_from_letter(text) result(wake)
      !! Map a wake category letter to its code, or zero if unknown.
      character(len=*), intent(in) :: text
         !! One of L, M, H, S, in either case.
      integer(int32) :: wake

      select case (text)
      case ("L", "l")
         wake = WAKE_LIGHT
      case ("M", "m")
         wake = WAKE_MEDIUM
      case ("H", "h")
         wake = WAKE_HEAVY
      case ("S", "s")
         wake = WAKE_SUPER
      case default
         wake = 0_int32
      end select
   end function wake_from_letter

   subroutine parse_clock_text(text, tick, err)
      !! Parse `HH:MM` or `HH:MM:SS` into a tick.
      !!
      !! Through `pic_tokenizer`'s `parse_int`, which is strict: "6x" is an
      !! error rather than 6. A list-directed `read` would accept a good deal
      !! more than a clock and differ between compilers about what.
      character(len=*), intent(in) :: text
         !! Clock text.
      integer(tick_k), intent(out) :: tick
         !! Milliseconds since midnight of the scenario day.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: hours, minutes, seconds
      integer(default_int) :: first_colon, second_colon
      type(error_t) :: parse_err

      tick = 0_tick_k
      first_colon = index(text, ":")
      if (first_colon <= 1_default_int) then
         call error_raise(err, ERROR_PARSE, "'"//text//"' is not HH:MM or HH:MM:SS")
         return
      end if

      seconds = 0_int32
      second_colon = index(text(first_colon + 1:), ":")

      call parse_int(text(1:first_colon - 1), hours, parse_err)
      if (second_colon == 0_default_int) then
         if (.not. parse_err%has_error()) call parse_int(text(first_colon + 1:), minutes, parse_err)
      else
         second_colon = first_colon + second_colon
         if (.not. parse_err%has_error()) then
            call parse_int(text(first_colon + 1:second_colon - 1), minutes, parse_err)
         end if
         if (.not. parse_err%has_error()) then
            call parse_int(text(second_colon + 1:), seconds, parse_err)
         end if
      end if

      if (parse_err%has_error()) then
         call error_raise(err, ERROR_PARSE, "'"//text//"' is not a clock time")
         return
      end if

      tick = int(hours, tick_k)*HOUR + int(minutes, tick_k)*MINUTE + int(seconds, tick_k)*SECOND
   end subroutine parse_clock_text

   pure function zero_padded(value) result(text)
      !! A bank index as exactly three digits, so callsigns sort as they read.
      integer(int32), intent(in) :: value
         !! Index, 1 to 999.
      character(len=3) :: text

      integer(int32) :: rest

      rest = modulo(value, 1000_int32)
      text(1:1) = achar(iachar("0") + rest/100_int32)
      text(2:2) = achar(iachar("0") + modulo(rest/10_int32, 10_int32))
      text(3:3) = achar(iachar("0") + modulo(rest, 10_int32))
   end function zero_padded

end module app_schedule
