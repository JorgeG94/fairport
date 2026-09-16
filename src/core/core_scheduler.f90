! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The temporal spine: what happens next, in what order.
module core_scheduler
   !! A min-heap keyed on sim time, over a pool of event records.
   !!
   !! The design document budgets about eighty lines for a hand-rolled binary
   !! heap. `pic_heap` is that heap, already written and already tested, and it
   !! breaks ties on equal keys by insertion order -- which is exactly the
   !! `seq` rule the event model needs, enforced by the container rather than
   !! by every caller remembering to supply it. So the heap holds
   !! `(tick, slot)` pairs and this module holds the records those slots point
   !! at. The `seq` field is still written into every event, because the log
   !! and any future checkpoint need to see it.
   !!
   !! Slots are never reused within a run. A cancelled event is tombstoned
   !! rather than removed (`core_world`'s generation counters), so recycling
   !! slots would buy nothing and would make a stale handle ambiguous.
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use core_event, only: event_t
   use pic_types, only: default_int
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_VALIDATION
   use pic_heap, only: heap_t
   implicit none
   private

   public :: scheduler_t

   integer(default_int), parameter :: INITIAL_CAPACITY = 256_default_int
      !! Event slots reserved on first use; grown by doubling.

   type :: scheduler_t
      !! The event queue and the records behind it.
      private
      type(heap_t) :: queue
         !! `(tick, slot)` pairs, min-ordered, FIFO on equal ticks.
      integer(tick_k), allocatable :: pool_tick(:)
      integer(int64), allocatable :: pool_seq(:)
      integer(int16), allocatable :: pool_kind(:)
      integer(id_k), allocatable :: pool_entity(:)
      integer(int32), allocatable :: pool_generation(:)
      integer(int64), allocatable :: pool_payload(:)
         !! The event pool, one array per field.
      integer(default_int) :: n_slots = 0_default_int
         !! Slots handed out so far.
      integer(default_int) :: cap = 0_default_int
         !! Slots the pool has room for.
      integer(int64) :: next_seq = 0_int64
         !! Monotonic sequence counter.
   contains
      procedure :: push => scheduler_push
      procedure :: pop => scheduler_pop
      procedure :: is_empty => scheduler_is_empty
      procedure :: next_tick => scheduler_next_tick
      procedure :: pending => scheduler_pending
      procedure :: scheduled_total => scheduler_scheduled_total
      procedure :: destroy => scheduler_destroy
   end type scheduler_t

contains

   subroutine scheduler_push(this, at, kind, entity, generation, payload, err)
      !! Schedule an event.
      !!
      !! Called with keyword arguments throughout the systems, which is
      !! Fortran's usable substitute for the strong handle types the C++ design
      !! gets from the type system: `call sched%push(at=bingo, kind=K_BINGOFUEL,
      !! entity=aircraft)` cannot be silently transposed.
      class(scheduler_t), intent(inout) :: this
      integer(tick_k), intent(in) :: at
         !! Sim time the event fires at.
      integer(int16), intent(in) :: kind
         !! One of the `K_*` identifiers.
      integer(id_k), intent(in) :: entity
         !! Entity the event is about, or `NO_ID`.
      integer(int32), intent(in), optional :: generation
         !! Tombstone stamp; omit for events that cannot go stale.
      integer(int64), intent(in), optional :: payload
         !! Kind-dependent scalar.
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: slot

      if (at < 0_tick_k) then
         call error_raise(err, ERROR_VALIDATION, "scheduler_push: negative tick")
         return
      end if

      call grow_if_needed(this, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      this%n_slots = this%n_slots + 1_default_int
      slot = this%n_slots

      this%pool_tick(slot) = at
      this%pool_seq(slot) = this%next_seq
      this%pool_kind(slot) = kind
      this%pool_entity(slot) = entity
      this%pool_generation(slot) = 0_int32
      this%pool_payload(slot) = 0_int64
      if (present(generation)) this%pool_generation(slot) = generation
      if (present(payload)) this%pool_payload(slot) = payload

      this%next_seq = this%next_seq + 1_int64

      call this%queue%push(at, int(slot, int32))
   end subroutine scheduler_push

   subroutine scheduler_pop(this, event, err)
      !! Remove and return the earliest event.
      class(scheduler_t), intent(inout) :: this
      type(event_t), intent(out) :: event
         !! The event; left at its defaults when the queue is empty.
      type(error_t), intent(inout), optional :: err

      integer(int64) :: key
      integer(int32) :: payload
      integer(default_int) :: slot

      if (this%queue%is_empty()) then
         call error_raise(err, ERROR_VALIDATION, "scheduler_pop: queue is empty")
         return
      end if

      call this%queue%pop(key, payload)
      slot = int(payload, default_int)

      event%tick = this%pool_tick(slot)
      event%seq = this%pool_seq(slot)
      event%kind = this%pool_kind(slot)
      event%entity = this%pool_entity(slot)
      event%generation = this%pool_generation(slot)
      event%payload = this%pool_payload(slot)
   end subroutine scheduler_pop

   pure function scheduler_is_empty(this) result(empty)
      !! Whether anything is still scheduled.
      class(scheduler_t), intent(in) :: this
      logical :: empty

      empty = this%queue%is_empty()
   end function scheduler_is_empty

   function scheduler_next_tick(this) result(tick)
      !! Sim time of the earliest pending event, or -1 when none is pending.
      class(scheduler_t), intent(in) :: this
      integer(tick_k) :: tick

      integer(int64) :: key
      integer(int32) :: payload

      tick = -1_tick_k
      if (this%queue%is_empty()) return
      call this%queue%peek(key, payload)
      tick = key
   end function scheduler_next_tick

   pure function scheduler_pending(this) result(n)
      !! Events currently in the queue, including any that will prove stale.
      class(scheduler_t), intent(in) :: this
      integer(default_int) :: n

      n = this%queue%size()
   end function scheduler_pending

   pure function scheduler_scheduled_total(this) result(n)
      !! Events scheduled since the run began.
      class(scheduler_t), intent(in) :: this
      integer(default_int) :: n

      n = this%n_slots
   end function scheduler_scheduled_total

   subroutine grow_if_needed(this, err)
      !! Double the event pool when it is full.
      type(scheduler_t), intent(inout) :: this
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: wanted
      integer(tick_k), allocatable :: new_tick(:)
      integer(int64), allocatable :: new_seq(:), new_payload(:)
      integer(int16), allocatable :: new_kind(:)
      integer(id_k), allocatable :: new_entity(:)
      integer(int32), allocatable :: new_generation(:)
      integer :: status

      if (this%n_slots < this%cap) return

      wanted = max(INITIAL_CAPACITY, 2_default_int*this%cap)
      allocate (new_tick(wanted), new_seq(wanted), new_kind(wanted), &
                new_entity(wanted), new_generation(wanted), new_payload(wanted), &
                stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "scheduler: event pool allocation failed")
         return
      end if

      if (this%cap > 0_default_int) then
         new_tick(1:this%cap) = this%pool_tick
         new_seq(1:this%cap) = this%pool_seq
         new_kind(1:this%cap) = this%pool_kind
         new_entity(1:this%cap) = this%pool_entity
         new_generation(1:this%cap) = this%pool_generation
         new_payload(1:this%cap) = this%pool_payload
      end if

      call move_alloc(new_tick, this%pool_tick)
      call move_alloc(new_seq, this%pool_seq)
      call move_alloc(new_kind, this%pool_kind)
      call move_alloc(new_entity, this%pool_entity)
      call move_alloc(new_generation, this%pool_generation)
      call move_alloc(new_payload, this%pool_payload)
      this%cap = wanted
   end subroutine grow_if_needed

   subroutine scheduler_destroy(this)
      !! Release the queue and the pool.
      class(scheduler_t), intent(inout) :: this

      call this%queue%destroy()
      if (allocated(this%pool_tick)) deallocate (this%pool_tick)
      if (allocated(this%pool_seq)) deallocate (this%pool_seq)
      if (allocated(this%pool_kind)) deallocate (this%pool_kind)
      if (allocated(this%pool_entity)) deallocate (this%pool_entity)
      if (allocated(this%pool_generation)) deallocate (this%pool_generation)
      if (allocated(this%pool_payload)) deallocate (this%pool_payload)
      this%n_slots = 0_default_int
      this%cap = 0_default_int
      this%next_seq = 0_int64
   end subroutine scheduler_destroy

end module core_scheduler
