! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Kind parameters, the null handle and the fixed bounds of the core.
module core_kinds
   !! Every kind in canonical simulation state is fixed width.
   !!
   !! This is the one place fairport departs from pic's "always `default_int`"
   !! rule, and it departs on purpose. `default_int` changes width with
   !! `PIC_DEFAULT_INT8`, and a tick or a handle that changes width changes
   !! what a saved command log means. Sizes, counts and loop indices are local
   !! bookkeeping and stay `default_int`; anything that reaches simulation
   !! state, a checkpoint or a hash is nailed down here.
   use pic_types, only: int8, int16, int32, int64
   implicit none
   private

   public :: int8, int16, int32, int64
   public :: tick_k, id_k
   public :: NO_ID

   integer, parameter :: tick_k = int64
      !! Sim time, in milliseconds. Sixty-four bits gives about 292 million
      !! years of range; the 32-bit alternative wraps after 25 days of sim
      !! time, which a long campaign reaches.

   integer, parameter :: id_k = int32
      !! Entity handles: one-based indices into the parallel arrays.

   integer(id_k), parameter :: NO_ID = 0_id_k
      !! The null handle. Zero rather than the C++ design's -1, because arrays
      !! here are one-based and `if (gate == NO_ID)` is then the natural test.

   ! The longest taxi route lives in `core_aircraft` as `AIRCRAFT_ROUTE_ROWS`,
   ! generated from the same field list as the route matrix itself and written
   ! into the checkpoint schema. Defining it twice is how the two drift.

end module core_kinds
