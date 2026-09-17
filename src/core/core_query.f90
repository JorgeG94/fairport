! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The read-only query layer.
module core_query
   !! Flat structs out, nothing in.
   !!
   !! Every query is `pure` and takes the world `intent(in)`. That pair is a
   !! compiler-checked guarantee that presentation cannot reach back and change
   !! the simulation -- stronger than C++ `const`, which promises only that
   !! this particular reference will not be used to write.
   !!
   !! Building the terminal against this from the first commit is what makes a
   !! graphical client later a second consumer of the same queries rather than
   !! a rewrite. It is also where the eventual `bind(c)` boundary goes.
   use core_kinds, only: tick_k, id_k, int32, NO_ID
   use core_world, only: world_t, CALLSIGN_LEN
   use pic_types, only: default_int
   implicit none
   private

   public :: aircraft_view_t
   public :: gate_view_t
   public :: query_aircraft
   public :: query_gates

   type :: aircraft_view_t
      !! What a display needs to know about one aircraft.
      integer(id_k) :: id = NO_ID
         !! Handle, so a command can refer back to it.
      character(len=CALLSIGN_LEN) :: callsign = ""
         !! Callsign as shown on the board.
      integer(int32) :: phase = 0_int32
         !! One of the `PHASE_*` codes.
      integer(int32) :: wake = 0_int32
         !! One of the `WAKE_*` codes.
      integer(id_k) :: node = NO_ID
         !! Taxiway node the aircraft is at.
      integer(id_k) :: gate = NO_ID
         !! Assigned stand, or `NO_ID`.
      integer(tick_k) :: touchdown_tick = 0_tick_k
         !! When it landed.
      integer(tick_k) :: on_blocks_tick = 0_tick_k
         !! When it parked, or zero if it has not.
      integer(int32) :: pax = 0_int32
         !! Passengers on board.
      integer(tick_k) :: airborne_tick = 0_tick_k
         !! When it left the ground, or zero if it has not.
      integer(tick_k) :: delay_ms = 0_tick_k
         !! Ready to airborne: every minute the airfield cost this departure.
      integer(int32) :: held = 0_int32
         !! Non-zero while the player is holding it on its stand.
      integer(int32) :: hold_ordered = 0_int32
         !! Non-zero while the player is holding it out of the landing order.
      integer(int32) :: holds = 0_int32
         !! Holding circuits flown waiting for a landing clearance.
      integer(tick_k) :: stand_wait_ms = 0_tick_k
         !! Time on the ground with nowhere to park.
   end type aircraft_view_t

   type :: gate_view_t
      !! What a display needs to know about one stand.
      integer(id_k) :: id = NO_ID
         !! Handle.
      character(len=CALLSIGN_LEN) :: name = ""
         !! Stand label.
      integer(id_k) :: occupant = NO_ID
         !! Aircraft on the stand, or `NO_ID`.
      integer(id_k) :: node = NO_ID
         !! Taxiway node the stand sits on.
   end type gate_view_t

contains

   pure subroutine query_aircraft(w, out, n)
      !! Fill `out` with one view per aircraft.
      !!
      !! The caller owns the buffer, so a display that refreshes twice a second
      !! allocates nothing. A buffer shorter than the aircraft count is filled
      !! as far as it goes and `n` says how far.
      type(world_t), intent(in) :: w
      type(aircraft_view_t), intent(inout) :: out(:)
         !! Caller-owned buffer.
      integer(default_int), intent(out) :: n
         !! Views written.

      integer(default_int) :: i

      n = min(w%aircraft%size(), int(size(out), default_int))
      do i = 1_default_int, n
         out(i)%id = int(i, id_k)
         out(i)%callsign = w%callsign(i)
         out(i)%phase = w%aircraft%phase(i)
         out(i)%wake = w%aircraft%wake(i)
         out(i)%node = w%aircraft%node(i)
         out(i)%gate = w%aircraft%gate(i)
         out(i)%touchdown_tick = w%aircraft%touchdown_tick(i)
         out(i)%on_blocks_tick = w%aircraft%on_blocks_tick(i)
         out(i)%pax = w%aircraft%pax(i)
         out(i)%airborne_tick = w%aircraft%airborne_tick(i)
         out(i)%delay_ms = w%aircraft%delay_ms(i)
         out(i)%held = w%aircraft%held(i)
         out(i)%hold_ordered = w%aircraft%hold_ordered(i)
         out(i)%holds = w%aircraft%holds(i)
         out(i)%stand_wait_ms = w%aircraft%stand_wait_ms(i)
      end do
   end subroutine query_aircraft

   pure subroutine query_gates(w, out, n)
      !! Fill `out` with one view per stand.
      type(world_t), intent(in) :: w
      type(gate_view_t), intent(inout) :: out(:)
         !! Caller-owned buffer.
      integer(default_int), intent(out) :: n
         !! Views written.

      integer(default_int) :: i

      n = min(int(w%n_gates, default_int), int(size(out), default_int))
      do i = 1_default_int, n
         out(i)%id = int(i, id_k)
         out(i)%name = w%gate_name(i)
         out(i)%occupant = w%gate_occupant(i)
         out(i)%node = w%gate_node(i)
      end do
   end subroutine query_gates

end module core_query
