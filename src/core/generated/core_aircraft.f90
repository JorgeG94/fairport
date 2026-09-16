! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Struct-of-arrays container for aircraft state.
!!
!! GENERATED FILE -- DO NOT EDIT.
!! Produced by `tools/autogen/core_aircraft.fypp`; edit the template and rerun
!! `tools/autogen/autogen.sh` instead.
module core_aircraft
   !! 15 scalar field arrays and 1 matrix field(s), serialized and hashed together.
   !!
   !! The schema string below is written into every stream and folded into
   !! every digest, so a checkpoint from a different field list fails loudly
   !! instead of decoding as garbage:
   !!
   !! ```
   !! aircraft;phase:i32,wake:i32,gate:i32,node:i32,goal:i32,generation:i32,route_len:i32,route_pos:i32,pa
   !! x:i32,touchdown_tick:i64,on_blocks_tick:i64,delay_ms:i64,held:i32,ready_tick:i64,airborne_tick:i64,r
   !! oute:i32x64
   !! ```
   !!
   !! prefixed by `SOA_SCHEMA_PREFIX`, which carries pic's own record-layout
   !! version. Fields are written and folded in exactly the order declared
   !! below, scalars first and matrices last.
   use core_kinds, only: int32, int64, id_k, NO_ID
   use pic_types, only: default_int
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_VALIDATION
   use pic_array_hash, only: array_hash64_t
   use pic_soa, only: SOA_SCHEMA_PREFIX, soa_write_prologue, soa_read_prologue, &
                      soa_write_field, soa_read_field, soa_hash_begin, soa_hash_field
   implicit none
   private

   public :: aircraft_soa_t
   public :: AIRCRAFT_SCHEMA
   public :: AIRCRAFT_ROUTE_ROWS

   character(len=*), parameter :: AIRCRAFT_SCHEMA = SOA_SCHEMA_PREFIX//"aircraft;phase:i32,wake:i32,gate:i32,node:i32,goal:i32,gene&
       &ration:i32,route_len:i32,route_pos:i32,pax:i32,touchdown_tick:i64,on_blocks_tick:i64,delay_ms:i64,held:i32,ready_tick:i64,a&
       &irborne_tick:i64,route:i32x64"
      !! Layout identity. Any change to the field list changes it, which
      !! retires every older checkpoint through the ordinary mismatch path.

   integer(default_int), parameter :: AIRCRAFT_ROUTE_ROWS = 64_default_int
      !! Rows of the `route` matrix. Part of the schema, so changing it
      !! retires old checkpoints rather than misreading them.

   type :: aircraft_soa_t
      !! Canonical aircraft state, one contiguous array per field.
      integer(int32), allocatable :: phase(:)
         !! Current phase of flight, one of the PHASE_* codes.
      integer(int32), allocatable :: wake(:)
         !! Wake turbulence category, one of the WAKE_* codes.
      integer(int32), allocatable :: gate(:)
         !! Assigned gate, or NO_ID.
      integer(int32), allocatable :: node(:)
         !! Taxiway node the aircraft is currently at.
      integer(int32), allocatable :: goal(:)
         !! Taxiway node it is routing to.
      integer(int32), allocatable :: generation(:)
         !! Tombstone counter; bumping it invalidates pending events.
      integer(int32), allocatable :: route_len(:)
         !! Number of nodes in the planned route.
      integer(int32), allocatable :: route_pos(:)
         !! Index into the route of the node last reached.
      integer(int32), allocatable :: pax(:)
         !! Passengers on board.
      integer(int64), allocatable :: touchdown_tick(:)
         !! Sim time of touchdown.
      integer(int64), allocatable :: on_blocks_tick(:)
         !! Sim time the aircraft parked.
      integer(int64), allocatable :: delay_ms(:)
         !! Accrued delay in milliseconds.
      integer(int32), allocatable :: held(:)
         !! Non-zero while the player is holding this departure on its stand.
      integer(int64), allocatable :: ready_tick(:)
         !! Sim time the turnaround finished and pushback was first wanted.
      integer(int64), allocatable :: airborne_tick(:)
         !! Sim time the departure left the ground.
      integer(int32), allocatable :: route(:, :)
         !! Planned taxi route, one column per aircraft. Shape (AIRCRAFT_ROUTE_ROWS, capacity).
      integer(default_int) :: n = 0_default_int
         !! Entities in use.
      integer(default_int) :: cap = 0_default_int
         !! Entities the arrays have room for.
   contains
      procedure :: reserve => aircraft_reserve
      procedure :: add => aircraft_add
      procedure :: size => aircraft_size
      procedure :: capacity => aircraft_capacity
      procedure :: serialize => aircraft_serialize
      procedure :: deserialize => aircraft_deserialize
      procedure :: state_hash64 => aircraft_state_hash64
      procedure :: destroy => aircraft_destroy
   end type aircraft_soa_t

contains

   subroutine aircraft_reserve(this, cap, err)
      !! Allocate room for `cap` entities, zeroed, discarding any contents.
      !!
      !! Sized once at load, as the design intends: core simulation state does
      !! not grow during a run, so there is no reallocation to make an event
      !! ordering depend on a heap address.
      class(aircraft_soa_t), intent(inout) :: this
      integer(default_int), intent(in) :: cap
         !! Entities to make room for; must not be negative.
      type(error_t), intent(inout), optional :: err

      integer :: status

      if (cap < 0_default_int) then
         call error_raise(err, ERROR_VALIDATION, "aircraft_reserve: negative capacity")
         return
      end if

      call this%destroy()

      allocate ( &
         this%phase(cap), &
         this%wake(cap), &
         this%gate(cap), &
         this%node(cap), &
         this%goal(cap), &
         this%generation(cap), &
         this%route_len(cap), &
         this%route_pos(cap), &
         this%pax(cap), &
         this%touchdown_tick(cap), &
         this%on_blocks_tick(cap), &
         this%delay_ms(cap), &
         this%held(cap), &
         this%ready_tick(cap), &
         this%airborne_tick(cap), &
         this%route(AIRCRAFT_ROUTE_ROWS, cap), &
         stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "aircraft_reserve: allocation failed")
         return
      end if

      this%phase = 0_int32
      this%wake = 0_int32
      this%gate = 0_int32
      this%node = 0_int32
      this%goal = 0_int32
      this%generation = 0_int32
      this%route_len = 0_int32
      this%route_pos = 0_int32
      this%pax = 0_int32
      this%touchdown_tick = 0_int64
      this%on_blocks_tick = 0_int64
      this%delay_ms = 0_int64
      this%held = 0_int32
      this%ready_tick = 0_int64
      this%airborne_tick = 0_int64
      this%route = NO_ID
      this%n = 0_default_int
      this%cap = cap
   end subroutine aircraft_reserve

   subroutine aircraft_add(this, index, err)
      !! Claim the next free slot and return its one-based index.
      !!
      !! Reports `ERROR_VALIDATION` rather than growing: exceeding the loaded
      !! capacity is a scenario error, and silently reallocating here would
      !! hide it until a checkpoint failed to match.
      class(aircraft_soa_t), intent(inout) :: this
      integer(id_k), intent(out) :: index
         !! One-based slot, or `NO_ID` when the container is full.
      type(error_t), intent(inout), optional :: err

      index = NO_ID
      if (this%n >= this%cap) then
         call error_raise(err, ERROR_VALIDATION, "aircraft_add: capacity exhausted")
         return
      end if

      this%n = this%n + 1_default_int
      index = int(this%n, id_k)
   end subroutine aircraft_add

   pure function aircraft_size(this) result(n)
      !! Number of entities in use.
      class(aircraft_soa_t), intent(in) :: this
      integer(default_int) :: n

      n = this%n
   end function aircraft_size

   pure function aircraft_capacity(this) result(cap)
      !! Number of entities the arrays have room for.
      class(aircraft_soa_t), intent(in) :: this
      integer(default_int) :: cap

      cap = this%cap
   end function aircraft_capacity

   subroutine aircraft_serialize(this, unit, err)
      !! Write the live slice of every field to an unformatted stream.
      !!
      !! `unit` must be open with `access='stream'`, `form='unformatted'`.
      class(aircraft_soa_t), intent(in) :: this
      integer, intent(in) :: unit
         !! Stream unit opened for writing.
      type(error_t), intent(inout), optional :: err

      type(error_t) :: fault
      integer(default_int) :: stream
      integer(int32), allocatable :: flat(:)
      integer :: status

      stream = int(unit, default_int)

      call soa_write_prologue(stream, AIRCRAFT_SCHEMA, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if

      call soa_write_field(stream, this%phase, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%wake, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%gate, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%node, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%goal, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%generation, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%route_len, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%route_pos, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%pax, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%touchdown_tick, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%on_blocks_tick, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%delay_ms, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%held, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%ready_tick, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_write_field(stream, this%airborne_tick, this%n, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if

      ! A matrix goes out as one flat record of ROWS*n elements, in Fortran
      ! column-major order, so reading it back is a reshape and not a guess.
      allocate (flat(AIRCRAFT_ROUTE_ROWS*this%n), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "aircraft_serialize: route buffer allocation failed")
         return
      end if
      flat = reshape(this%route(:, 1:this%n), [AIRCRAFT_ROUTE_ROWS*this%n])
      call soa_write_field(stream, flat, AIRCRAFT_ROUTE_ROWS*this%n, fault)
      deallocate (flat)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
   end subroutine aircraft_serialize

   subroutine aircraft_deserialize(this, unit, err)
      !! Read back what `serialize` wrote, into an already reserved container.
      !!
      !! The container keeps the capacity it was reserved with; only the live
      !! count changes. Reading straight into the field arrays would resize
      !! them to the stream's length and quietly break the rule that core
      !! state is sized once at load.
      class(aircraft_soa_t), intent(inout) :: this
      integer, intent(in) :: unit
         !! Stream unit opened for reading.
      type(error_t), intent(inout), optional :: err

      type(error_t) :: fault
      integer(default_int) :: stream, n
      logical :: swapped
      integer(int32), allocatable :: buffer_phase(:)
      integer(int32), allocatable :: buffer_wake(:)
      integer(int32), allocatable :: buffer_gate(:)
      integer(int32), allocatable :: buffer_node(:)
      integer(int32), allocatable :: buffer_goal(:)
      integer(int32), allocatable :: buffer_generation(:)
      integer(int32), allocatable :: buffer_route_len(:)
      integer(int32), allocatable :: buffer_route_pos(:)
      integer(int32), allocatable :: buffer_pax(:)
      integer(int64), allocatable :: buffer_touchdown_tick(:)
      integer(int64), allocatable :: buffer_on_blocks_tick(:)
      integer(int64), allocatable :: buffer_delay_ms(:)
      integer(int32), allocatable :: buffer_held(:)
      integer(int64), allocatable :: buffer_ready_tick(:)
      integer(int64), allocatable :: buffer_airborne_tick(:)
      integer(int32), allocatable :: buffer_route(:)

      stream = int(unit, default_int)

      call soa_read_prologue(stream, AIRCRAFT_SCHEMA, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      if (n > this%cap) then
         call error_raise(err, ERROR_VALIDATION, &
                          "aircraft_deserialize: stream holds more entities than the container was reserved for")
         return
      end if

      call soa_read_field(stream, "phase", buffer_phase, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "wake", buffer_wake, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "gate", buffer_gate, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "node", buffer_node, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "goal", buffer_goal, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "generation", buffer_generation, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "route_len", buffer_route_len, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "route_pos", buffer_route_pos, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "pax", buffer_pax, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "touchdown_tick", buffer_touchdown_tick, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "on_blocks_tick", buffer_on_blocks_tick, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "delay_ms", buffer_delay_ms, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "held", buffer_held, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "ready_tick", buffer_ready_tick, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if
      call soa_read_field(stream, "airborne_tick", buffer_airborne_tick, n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if

      call soa_read_field(stream, "route", buffer_route, &
                          AIRCRAFT_ROUTE_ROWS*n, swapped, fault)
      if (fault%has_error()) then
         if (present(err)) err = fault
         return
      end if

      ! Nothing is committed until every record has been read, so a truncated
      ! stream leaves the container as it was rather than half updated.
      this%phase(1:n) = buffer_phase
      this%wake(1:n) = buffer_wake
      this%gate(1:n) = buffer_gate
      this%node(1:n) = buffer_node
      this%goal(1:n) = buffer_goal
      this%generation(1:n) = buffer_generation
      this%route_len(1:n) = buffer_route_len
      this%route_pos(1:n) = buffer_route_pos
      this%pax(1:n) = buffer_pax
      this%touchdown_tick(1:n) = buffer_touchdown_tick
      this%on_blocks_tick(1:n) = buffer_on_blocks_tick
      this%delay_ms(1:n) = buffer_delay_ms
      this%held(1:n) = buffer_held
      this%ready_tick(1:n) = buffer_ready_tick
      this%airborne_tick(1:n) = buffer_airborne_tick
      this%route(:, 1:n) = reshape(buffer_route, [AIRCRAFT_ROUTE_ROWS, n])
      this%n = n
   end subroutine aircraft_deserialize

   function aircraft_state_hash64(this) result(digest)
      !! Fingerprint the live slice of every field, in declaration order.
      !!
      !! Sixty-four bits because these digests are used as identifiers, not
      !! only compared in pairs: among 10**5 distinct 32-bit digests some pair
      !! collides with probability about 69%, against 3e-10 at 64 bits.
      !!
      !! The byte stream is the one `pic_soa` defines, so a digest here means
      !! the same thing as a digest of any other pic SoA container.
      class(aircraft_soa_t), intent(in) :: this
      integer(int64) :: digest

      type(array_hash64_t) :: hasher
      integer(int32), allocatable :: flat(:)

      call soa_hash_begin(hasher, AIRCRAFT_SCHEMA, this%n)
      call soa_hash_field(hasher, this%phase, this%n)
      call soa_hash_field(hasher, this%wake, this%n)
      call soa_hash_field(hasher, this%gate, this%n)
      call soa_hash_field(hasher, this%node, this%n)
      call soa_hash_field(hasher, this%goal, this%n)
      call soa_hash_field(hasher, this%generation, this%n)
      call soa_hash_field(hasher, this%route_len, this%n)
      call soa_hash_field(hasher, this%route_pos, this%n)
      call soa_hash_field(hasher, this%pax, this%n)
      call soa_hash_field(hasher, this%touchdown_tick, this%n)
      call soa_hash_field(hasher, this%on_blocks_tick, this%n)
      call soa_hash_field(hasher, this%delay_ms, this%n)
      call soa_hash_field(hasher, this%held, this%n)
      call soa_hash_field(hasher, this%ready_tick, this%n)
      call soa_hash_field(hasher, this%airborne_tick, this%n)
      flat = reshape(this%route(:, 1:this%n), [AIRCRAFT_ROUTE_ROWS*this%n])
      call soa_hash_field(hasher, flat, AIRCRAFT_ROUTE_ROWS*this%n)
      digest = hasher%digest()
   end function aircraft_state_hash64

   subroutine aircraft_destroy(this)
      !! Release every array and reset the counts.
      class(aircraft_soa_t), intent(inout) :: this

      if (allocated(this%phase)) deallocate (this%phase)
      if (allocated(this%wake)) deallocate (this%wake)
      if (allocated(this%gate)) deallocate (this%gate)
      if (allocated(this%node)) deallocate (this%node)
      if (allocated(this%goal)) deallocate (this%goal)
      if (allocated(this%generation)) deallocate (this%generation)
      if (allocated(this%route_len)) deallocate (this%route_len)
      if (allocated(this%route_pos)) deallocate (this%route_pos)
      if (allocated(this%pax)) deallocate (this%pax)
      if (allocated(this%touchdown_tick)) deallocate (this%touchdown_tick)
      if (allocated(this%on_blocks_tick)) deallocate (this%on_blocks_tick)
      if (allocated(this%delay_ms)) deallocate (this%delay_ms)
      if (allocated(this%held)) deallocate (this%held)
      if (allocated(this%ready_tick)) deallocate (this%ready_tick)
      if (allocated(this%airborne_tick)) deallocate (this%airborne_tick)
      if (allocated(this%route)) deallocate (this%route)
      this%n = 0_default_int
      this%cap = 0_default_int
   end subroutine aircraft_destroy

end module core_aircraft
