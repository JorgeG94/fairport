! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Per-system random streams.
module core_rng
   !! One shared generator couples unrelated systems and breaks every saved
   !! scenario the moment a module is added: insert one draw in the weather
   !! system and every arrival time downstream shifts. Each system therefore
   !! draws from its own stream, derived from the master seed and a stable
   !! stream identifier.
   !!
   !! The identifiers below are as append-only as the event list, and for the
   !! same reason. Renumbering one silently changes every scenario that used
   !! it.
   !!
   !! The generators and the distributions are pic's: `splitmix64_t` is
   !! portable and exact, and `pic_random_dist`'s integer tier is pinned
   !! bit-for-bit across compilers by pic's own tests. Nothing in `core/` may
   !! name `pic_random_dist_real`, whose deviates go through libm and are
   !! therefore not reproducible; CI greps for it.
   use core_kinds, only: int32, int64
   use pic_types, only: default_int
   use pic_rng, only: splitmix64_t, stream_for
   implicit none
   private

   public :: stream_t
   public :: core_stream_for
   public :: STREAM_ARRIVAL, STREAM_DEPARTURE, STREAM_TAXI, STREAM_WEATHER
   public :: STREAM_INCIDENT, STREAM_ECONOMY, STREAM_PASSENGER

   ! Stream identifiers. APPEND ONLY.
   integer(default_int), parameter :: STREAM_ARRIVAL = 1_default_int
   integer(default_int), parameter :: STREAM_DEPARTURE = 2_default_int
   integer(default_int), parameter :: STREAM_TAXI = 3_default_int
   integer(default_int), parameter :: STREAM_WEATHER = 4_default_int
   integer(default_int), parameter :: STREAM_INCIDENT = 5_default_int
   integer(default_int), parameter :: STREAM_ECONOMY = 6_default_int
   integer(default_int), parameter :: STREAM_PASSENGER = 7_default_int

   type :: stream_t
      !! One system's generator.
      type(splitmix64_t) :: gen
         !! The generator itself. Public so that `pic_random_dist` can take it
         !! by reference, which is the whole point of holding one.
   end type stream_t

contains

   subroutine core_stream_for(master_seed, stream_id, stream)
      !! Build the generator belonging to `stream_id`.
      !!
      !! Stream 7 is the same sequence whether its system runs first, last, or
      !! is added next year.
      integer(int64), intent(in) :: master_seed
         !! The scenario's master seed.
      integer(default_int), intent(in) :: stream_id
         !! One of the `STREAM_*` identifiers.
      type(stream_t), intent(out) :: stream
         !! The derived generator.

      call stream_for(master_seed, stream_id, stream%gen)
   end subroutine core_stream_for

end module core_rng
