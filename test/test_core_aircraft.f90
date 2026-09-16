! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the generated struct-of-arrays container.
module test_core_aircraft
   !! The checkpoint format, exercised on purpose.
   !!
   !! The first version of this container hand-rolled its stream envelope and
   !! wrote the element count as `integer(default_int)`, whose width follows
   !! `PIC_DEFAULT_INT8`. A checkpoint written by one build was unreadable by
   !! the other. Nothing caught it, because nothing tested it. These tests are
   !! the thing that was missing, and `round_trip_count_width_is_fixed` is the
   !! one that would have.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_aircraft, only: aircraft_soa_t
   use core_kinds, only: id_k, int32, int64, NO_ID
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_core_aircraft_tests

contains

   subroutine collect_core_aircraft_tests(testsuite)
      !! Register the container tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("reserve_zeroes_everything", test_reserve_zeroes), &
                  new_unittest("add_stops_at_capacity", test_add_capacity), &
                  new_unittest("round_trip_restores_every_field", test_round_trip), &
                  new_unittest("round_trip_restores_the_route_matrix", test_round_trip_route), &
                  new_unittest("round_trip_count_width_is_fixed", test_count_width), &
                  new_unittest("deserialize_keeps_capacity", test_keeps_capacity), &
                  new_unittest("foreign_schema_is_rejected", test_foreign_schema), &
                  new_unittest("oversized_stream_is_rejected", test_oversized_stream), &
                  new_unittest("hash_follows_state", test_hash_follows_state), &
                  new_unittest("hash_ignores_dead_slots", test_hash_ignores_dead) &
                  ]
   end subroutine collect_core_aircraft_tests

   subroutine fill(container, n, err)
      !! A container with `n` entities and a distinct value in every field.
      type(aircraft_soa_t), intent(inout) :: container
      integer(default_int), intent(in) :: n
         !! Entities to create.
      type(error_t), intent(inout) :: err

      integer(default_int) :: i
      integer(id_k) :: slot

      call container%reserve(8_default_int, err)
      do i = 1_default_int, n
         call container%add(slot, err)
         container%phase(slot) = int(10 + i, int32)
         container%wake(slot) = int(20 + i, int32)
         container%gate(slot) = int(30 + i, int32)
         container%node(slot) = int(40 + i, int32)
         container%goal(slot) = int(50 + i, int32)
         container%generation(slot) = int(60 + i, int32)
         container%route_len(slot) = int(3, int32)
         container%route_pos(slot) = int(1, int32)
         container%pax(slot) = int(100 + i, int32)
         container%touchdown_tick(slot) = int(1000 + i, int64)
         container%on_blocks_tick(slot) = int(2000 + i, int64)
         container%delay_ms(slot) = int(-i, int64)
         container%route(1:3, slot) = [int(7 + i, int32), int(8 + i, int32), int(9 + i, int32)]
      end do
   end subroutine fill

   subroutine write_to(container, path, err)
      !! Serialize to a scratch file.
      type(aircraft_soa_t), intent(in) :: container
      character(len=*), intent(in) :: path
         !! File to write.
      type(error_t), intent(inout) :: err

      integer :: unit

      open (newunit=unit, file=path, access="stream", form="unformatted", &
            status="replace", action="write")
      call container%serialize(unit, err)
      close (unit)
   end subroutine write_to

   subroutine read_from(container, path, err)
      !! Deserialize from a scratch file.
      type(aircraft_soa_t), intent(inout) :: container
      character(len=*), intent(in) :: path
         !! File to read.
      type(error_t), intent(inout) :: err

      integer :: unit

      open (newunit=unit, file=path, access="stream", form="unformatted", &
            status="old", action="read")
      call container%deserialize(unit, err)
      close (unit)
   end subroutine read_from

   subroutine discard(path)
      !! Remove a scratch file.
      character(len=*), intent(in) :: path
         !! File to delete.

      integer :: unit

      open (newunit=unit, file=path, status="old", action="read")
      close (unit, status="delete")
   end subroutine discard

   subroutine test_reserve_zeroes(error)
      !! A fresh container is zero everywhere and empty.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: container
      type(error_t) :: err

      call container%reserve(4_default_int, err)
      call check(error,.not. err%has_error(), "reserve reported an error")
      if (allocated(error)) return

      call check(error, container%size() == 0_default_int, "a fresh container is not empty")
      if (allocated(error)) return
      call check(error, container%capacity() == 4_default_int, "wrong capacity")
      if (allocated(error)) return
      call check(error, all(container%phase == 0_int32), "phase was not zeroed")
      if (allocated(error)) return
      call check(error, all(container%touchdown_tick == 0_int64), "touchdown_tick was not zeroed")
      if (allocated(error)) return
      call check(error, all(container%route == NO_ID), "route was not cleared")

      call container%destroy()
   end subroutine test_reserve_zeroes

   subroutine test_add_capacity(error)
      !! Exceeding the reserved capacity is an error, not a silent growth.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: container
      type(error_t) :: err, overflow
      integer(id_k) :: slot
      integer(default_int) :: i

      call container%reserve(2_default_int, err)
      do i = 1_default_int, 2_default_int
         call container%add(slot, err)
      end do
      call check(error,.not. err%has_error(), "filling to capacity should not error")
      if (allocated(error)) return

      call container%add(slot, overflow)
      call check(error, overflow%has_error(), "exceeding capacity should be an error")
      if (allocated(error)) return
      call check(error, slot == NO_ID, "a failed add should return NO_ID")
      if (allocated(error)) return
      call check(error, container%size() == 2_default_int, "a failed add changed the size")

      call container%destroy()
   end subroutine test_add_capacity

   subroutine test_round_trip(error)
      !! Every scalar field survives a write and a read.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: written, restored
      type(error_t) :: err
      character(len=*), parameter :: path = "test_aircraft_round_trip.bin"

      call fill(written, 3_default_int, err)
      call write_to(written, path, err)
      call check(error,.not. err%has_error(), "serialize reported an error")
      if (allocated(error)) return

      call restored%reserve(8_default_int, err)
      call read_from(restored, path, err)
      call check(error,.not. err%has_error(), "deserialize reported an error")
      if (allocated(error)) return

      call check(error, restored%size() == 3_default_int, "live count did not survive")
      if (allocated(error)) return
      call check(error, all(restored%phase(1:3) == written%phase(1:3)), "phase")
      if (allocated(error)) return
      call check(error, all(restored%pax(1:3) == written%pax(1:3)), "pax")
      if (allocated(error)) return
      call check(error, all(restored%touchdown_tick(1:3) == written%touchdown_tick(1:3)), "touchdown_tick")
      if (allocated(error)) return
      call check(error, all(restored%delay_ms(1:3) == written%delay_ms(1:3)), "delay_ms, which is negative")
      if (allocated(error)) return
      call check(error, restored%state_hash64() == written%state_hash64(), &
                 "a round trip changed the state digest")

      call discard(path)
      call written%destroy()
      call restored%destroy()
   end subroutine test_round_trip

   subroutine test_round_trip_route(error)
      !! The route matrix survives too.
      !!
      !! It is rank 2, so it goes out as one flat column-major record. Getting
      !! the reshape backwards would still round-trip a square matrix, which is
      !! why the fixture writes three distinct values per column.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: written, restored
      type(error_t) :: err
      character(len=*), parameter :: path = "test_aircraft_route.bin"

      call fill(written, 3_default_int, err)
      call write_to(written, path, err)
      call restored%reserve(8_default_int, err)
      call read_from(restored, path, err)
      call check(error,.not. err%has_error(), "round trip reported an error")
      if (allocated(error)) return

      call check(error, all(restored%route(:, 1:3) == written%route(:, 1:3)), &
                 "the route matrix did not survive")
      if (allocated(error)) return
      call check(error, restored%route(2, 2) == written%route(2, 2), "route transposed")

      call discard(path)
      call written%destroy()
      call restored%destroy()
   end subroutine test_round_trip_route

   subroutine test_count_width(error)
      !! The stream length is a constant of the field list, not of the build.
      !!
      !! This is the regression test for the bug that prompted this suite. The
      !! first version of this container wrote the element count as
      !! `integer(default_int)`, so the stream grew by four bytes under
      !! `PIC_DEFAULT_INT8=ON` and every record after the count misaligned.
      !! The count now goes out through `soa_write_prologue` as an explicit
      !! `int64`, and every field record carries its own width.
      !!
      !! Pinning the byte length is what makes that testable from inside one
      !! build: nothing here varies with `default_int` any more, so if this
      !! number moves between the two integer widths, the cross-build
      !! compatibility this asserts has been lost. Change it only when the
      !! field list changes -- which also changes the schema, and so retires
      !! old checkpoints anyway.
      type(error_type), allocatable, intent(out) :: error

      integer(int64), parameter :: EXPECTED_BYTES = 1133_int64
         !! Length of a stream holding exactly two aircraft.

      type(aircraft_soa_t) :: container
      type(error_t) :: err
      character(len=*), parameter :: path = "test_aircraft_width.bin"
      integer(int64) :: bytes, again

      call fill(container, 2_default_int, err)
      call write_to(container, path, err)
      call check(error,.not. err%has_error(), "serialize reported an error")
      if (allocated(error)) return

      inquire (file=path, size=bytes)
      call check(error, bytes == EXPECTED_BYTES, &
                 "the serialized length moved; if this build changed default_int, "// &
                 "the checkpoint format is no longer portable between the two")
      if (allocated(error)) then
         call discard(path)
         call container%destroy()
         return
      end if

      ! Writing the same container twice must give the same length, which fails
      ! if any record length depends on something other than the field list.
      call write_to(container, path, err)
      inquire (file=path, size=again)
      call check(error, again == bytes, "two writes of one container differ in length")

      call discard(path)
      call container%destroy()
   end subroutine test_count_width

   subroutine test_keeps_capacity(error)
      !! Reading a short stream does not shrink the arrays.
      !!
      !! Core state is sized once at load. If `deserialize` reallocated the
      !! field arrays to the stream's length, a restored world would silently
      !! have no room for the aircraft still to arrive.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: written, restored
      type(error_t) :: err
      character(len=*), parameter :: path = "test_aircraft_capacity.bin"

      call fill(written, 2_default_int, err)
      call write_to(written, path, err)

      call restored%reserve(8_default_int, err)
      call read_from(restored, path, err)
      call check(error,.not. err%has_error(), "deserialize reported an error")
      if (allocated(error)) return

      call check(error, restored%size() == 2_default_int, "wrong live count")
      if (allocated(error)) return
      call check(error, restored%capacity() == 8_default_int, "capacity was shrunk by the read")
      if (allocated(error)) return
      call check(error, int(size(restored%phase), default_int) == 8_default_int, &
                 "a field array was resized to the stream length")

      call discard(path)
      call written%destroy()
      call restored%destroy()
   end subroutine test_keeps_capacity

   subroutine test_foreign_schema(error)
      !! A stream from another layout fails loudly.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: container
      type(error_t) :: err, fault
      character(len=*), parameter :: path = "test_aircraft_foreign.bin"
      integer :: unit

      ! A well-formed pic stream carrying a schema that is not ours.
      open (newunit=unit, file=path, access="stream", form="unformatted", &
            status="replace", action="write")
      write (unit) "not the aircraft schema at all, not even close"
      close (unit)

      call container%reserve(4_default_int, err)
      call read_from(container, path, fault)
      call check(error, fault%has_error(), "a foreign stream was accepted")
      if (allocated(error)) return
      call check(error, container%size() == 0_default_int, "a rejected read changed the container")

      call discard(path)
      call container%destroy()
   end subroutine test_foreign_schema

   subroutine test_oversized_stream(error)
      !! A stream with more entities than the container was reserved for fails.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: written, restored
      type(error_t) :: err, fault
      character(len=*), parameter :: path = "test_aircraft_oversized.bin"

      call fill(written, 5_default_int, err)
      call write_to(written, path, err)

      call restored%reserve(2_default_int, err)
      call read_from(restored, path, fault)
      call check(error, fault%has_error(), "an oversized stream was accepted")
      if (allocated(error)) return
      call check(error, restored%size() == 0_default_int, "a rejected read changed the live count")

      call discard(path)
      call written%destroy()
      call restored%destroy()
   end subroutine test_oversized_stream

   subroutine test_hash_follows_state(error)
      !! The digest changes when any field does, and only then.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: first, second
      type(error_t) :: err
      integer(int64) :: before

      call fill(first, 3_default_int, err)
      call fill(second, 3_default_int, err)
      call check(error, first%state_hash64() == second%state_hash64(), &
                 "two identical containers disagree")
      if (allocated(error)) return

      before = second%state_hash64()
      second%pax(2) = second%pax(2) + 1_int32
      call check(error, second%state_hash64() /= before, "changing pax did not change the digest")
      if (allocated(error)) return

      before = second%state_hash64()
      second%route(1, 2) = second%route(1, 2) + 1_int32
      call check(error, second%state_hash64() /= before, &
                 "changing the route did not change the digest")

      call first%destroy()
      call second%destroy()
   end subroutine test_hash_follows_state

   subroutine test_hash_ignores_dead(error)
      !! Slots past the live count do not reach the digest.
      !!
      !! Otherwise a container's fingerprint would depend on how much room it
      !! happened to be given, and two runs that reserved differently would
      !! look like a divergence.
      type(error_type), allocatable, intent(out) :: error

      type(aircraft_soa_t) :: container
      type(error_t) :: err
      integer(int64) :: before

      call fill(container, 2_default_int, err)
      before = container%state_hash64()

      container%pax(5) = 999_int32
      container%route(1, 6) = 999_int32

      call check(error, container%state_hash64() == before, &
                 "a slot past the live count reached the digest")

      call container%destroy()
   end subroutine test_hash_ignores_dead

end module test_core_aircraft
