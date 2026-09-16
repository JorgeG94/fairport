! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Space-time reservations over the taxiway graph.
module core_reservations
   !! Which aircraft holds which node, and when.
   !!
   !! ## Why this exists
   !!
   !! Until now two aircraft could occupy the same taxiway node at the same
   !! instant and nothing noticed. With four movements a day that was a
   !! theoretical problem; with forty arrivals through a single apron entrance
   !! it is the apron entrance.
   !!
   !! ## Deliberately mediocre
   !!
   !! Conflict-free ground movement is multi-agent pathfinding, which is
   !! NP-hard in general, and chasing optimality here would be a mistake twice
   !! over. Real ground controllers run greedy heuristics with a few minutes of
   !! lookahead and no guarantees, and the result is exactly the
   !! mediocre-but-functional throughput a player would recognise.
   !!
   !! More importantly, **the player is the optimizer**. They improve
   !! throughput by building better topology, by staggering the schedule they
   !! accept, by holding one departure so another goes first. A solver good
   !! enough to route around every conflict would quietly delete the game.
   !!
   !! So: prioritized planning. An aircraft plans a route, checks whether every
   !! node on it is free for the window it needs, and either commits to the
   !! whole thing or waits and asks again. Where that produces a dumb outcome,
   !! that is not a bug -- it is the delay the player has to manage.
   !!
   !! ## The layout
   !!
   !! One flat pool of reservations with an intrusive linked list per node, and
   !! a free list through the same `next` array. No allocation after load, no
   !! generic container, and the iteration order over a node's reservations is
   !! a function of insertion order alone -- which is what keeps two builds
   !! agreeing about which aircraft got the apron entrance.
   use core_kinds, only: tick_k, id_k, int32, int64, NO_ID
   use pic_types, only: default_int
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_VALIDATION
   implicit none
   private

   public :: reservation_pool_t

   integer(int32), parameter :: NO_ENTRY = 0_int32
      !! End-of-list marker. Zero is never a valid slot, because slots are
      !! one-based.

   type :: reservation_pool_t
      !! Every live reservation, and the per-node lists threading through them.
      private
      integer(id_k), allocatable :: node(:)
         !! Node each reservation is against.
      integer(tick_k), allocatable :: from(:)
         !! First instant of the window, inclusive.
      integer(tick_k), allocatable :: to(:)
         !! Last instant of the window, exclusive.
      integer(id_k), allocatable :: who(:)
         !! Aircraft holding it.
      integer(int32), allocatable :: next(:)
         !! Intrusive link: next reservation on this node, or the next free
         !! slot when the entry is on the free list.
      integer(int32), allocatable :: head(:)
         !! First reservation on each node, or `NO_ENTRY`.
      integer(int32) :: free_head = NO_ENTRY
         !! First unused slot.
      integer(default_int) :: cap = 0_default_int
         !! Slots in the pool.
      integer(default_int) :: live = 0_default_int
         !! Slots currently in use.
   contains
      procedure :: reserve_pool => pool_reserve_pool
      procedure :: is_free => pool_is_free
      procedure :: claim => pool_claim
      procedure :: release_all => pool_release_all
      procedure :: held_by => pool_held_by
      procedure :: in_use => pool_in_use
      procedure :: capacity => pool_capacity
      procedure :: destroy => pool_destroy
   end type reservation_pool_t

contains

   subroutine pool_reserve_pool(this, n_nodes, cap, err)
      !! Size the pool once, at load.
      class(reservation_pool_t), intent(inout) :: this
      integer(int32), intent(in) :: n_nodes
         !! Nodes in the taxiway graph.
      integer(default_int), intent(in) :: cap
         !! Reservations the pool can hold at once.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: i
      integer :: status

      if (n_nodes < 0_int32 .or. cap < 0_default_int) then
         call error_raise(err, ERROR_VALIDATION, "reservation_pool: negative size")
         return
      end if

      call this%destroy()

      allocate (this%node(cap), this%from(cap), this%to(cap), this%who(cap), &
                this%next(cap), this%head(n_nodes), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "reservation_pool: allocation failed")
         return
      end if

      this%node = NO_ID
      this%from = 0_tick_k
      this%to = 0_tick_k
      this%who = NO_ID
      this%head = NO_ENTRY

      ! Thread every slot onto the free list, lowest first, so that the slot a
      ! claim gets is a function of how many are in use and nothing else.
      do i = 1_int32, int(cap, int32) - 1_int32
         this%next(i) = i + 1_int32
      end do
      if (cap > 0_default_int) this%next(cap) = NO_ENTRY
      this%free_head = merge(1_int32, NO_ENTRY, cap > 0_default_int)

      this%cap = cap
      this%live = 0_default_int
   end subroutine pool_reserve_pool

   pure function pool_is_free(this, node, from, to, ignoring) result(free)
      !! Whether a node is unclaimed for the whole of `[from, to)`.
      !!
      !! Half-open on purpose: an aircraft leaving a node at exactly the
      !! instant the next one arrives is a handover, not a conflict, and
      !! treating it as one would deadlock a single-entrance apron.
      class(reservation_pool_t), intent(in) :: this
      integer(id_k), intent(in) :: node
         !! Node to test.
      integer(tick_k), intent(in) :: from
         !! Start of the window, inclusive.
      integer(tick_k), intent(in) :: to
         !! End of the window, exclusive.
      integer(id_k), intent(in) :: ignoring
         !! Aircraft whose own reservations do not count, or `NO_ID`. A replan
         !! must not conflict with the route it is replacing.
      logical :: free

      integer(int32) :: entry

      ! An unsized pool constrains nothing, which is exactly the behaviour
      ! before reservations existed. `size` of an unallocated array is not
      ! merely zero, it is undefined, so this guard comes first.
      free = .true.
      if (.not. allocated(this%head)) return
      if (node < 1_id_k .or. int(node, default_int) > int(size(this%head), default_int)) return

      entry = this%head(node)
      do while (entry /= NO_ENTRY)
         if (this%who(entry) /= ignoring) then
            if (from < this%to(entry) .and. this%from(entry) < to) then
               free = .false.
               return
            end if
         end if
         entry = this%next(entry)
      end do
   end function pool_is_free

   subroutine pool_claim(this, node, from, to, who, err)
      !! Take a node for a window.
      !!
      !! The caller has already checked with `is_free`. Claiming an overlapping
      !! window is not rejected here: a route is checked whole and then claimed
      !! whole, and re-walking every node twice inside the claim would cost
      !! more than it catches.
      class(reservation_pool_t), intent(inout) :: this
      integer(id_k), intent(in) :: node
         !! Node to claim.
      integer(tick_k), intent(in) :: from
         !! Start of the window, inclusive.
      integer(tick_k), intent(in) :: to
         !! End of the window, exclusive.
      integer(id_k), intent(in) :: who
         !! Aircraft taking it.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: entry

      if (.not. allocated(this%head)) then
         call error_raise(err, ERROR_VALIDATION, "reservation_pool: claim on a pool that was never sized")
         return
      end if
      if (node < 1_id_k .or. int(node, default_int) > int(size(this%head), default_int)) then
         call error_raise(err, ERROR_VALIDATION, "reservation_pool: node outside the graph")
         return
      end if
      if (this%free_head == NO_ENTRY) then
         ! Sized at load from the aircraft capacity and the longest route, so
         ! running out means one of those assumptions is wrong rather than that
         ! the airport is busy.
         call error_raise(err, ERROR_ALLOC, "reservation_pool: exhausted")
         return
      end if

      entry = this%free_head
      this%free_head = this%next(entry)

      this%node(entry) = node
      this%from(entry) = from
      this%to(entry) = to
      this%who(entry) = who

      this%next(entry) = this%head(node)
      this%head(node) = entry
      this%live = this%live + 1_default_int
   end subroutine pool_claim

   subroutine pool_release_all(this, who)
      !! Give back everything one aircraft holds.
      !!
      !! Called when a route completes or is abandoned. Walking every node is
      !! affordable because the graph is small and this happens once per
      !! movement, and it means an aircraft never has to remember which slots
      !! were its own.
      class(reservation_pool_t), intent(inout) :: this
      integer(id_k), intent(in) :: who
         !! Aircraft to release.

      integer(int32) :: node, entry, following, previous

      if (.not. allocated(this%head)) return

      do node = 1_int32, int(size(this%head), int32)
         previous = NO_ENTRY
         entry = this%head(node)
         do while (entry /= NO_ENTRY)
            following = this%next(entry)
            if (this%who(entry) == who) then
               if (previous == NO_ENTRY) then
                  this%head(node) = following
               else
                  this%next(previous) = following
               end if
               this%who(entry) = NO_ID
               this%node(entry) = NO_ID
               this%next(entry) = this%free_head
               this%free_head = entry
               this%live = this%live - 1_default_int
            else
               previous = entry
            end if
            entry = following
         end do
      end do
   end subroutine pool_release_all

   pure function pool_held_by(this, node, at) result(who)
      !! Which aircraft holds a node at an instant, or `NO_ID`.
      class(reservation_pool_t), intent(in) :: this
      integer(id_k), intent(in) :: node
         !! Node to query.
      integer(tick_k), intent(in) :: at
         !! Instant to query.
      integer(id_k) :: who

      integer(int32) :: entry

      who = NO_ID
      if (.not. allocated(this%head)) return
      if (node < 1_id_k .or. int(node, default_int) > int(size(this%head), default_int)) return

      entry = this%head(node)
      do while (entry /= NO_ENTRY)
         if (at >= this%from(entry) .and. at < this%to(entry)) then
            who = this%who(entry)
            return
         end if
         entry = this%next(entry)
      end do
   end function pool_held_by

   pure function pool_in_use(this) result(n)
      !! Reservations currently held.
      class(reservation_pool_t), intent(in) :: this
      integer(default_int) :: n

      n = this%live
   end function pool_in_use

   pure function pool_capacity(this) result(n)
      !! Reservations the pool can hold.
      class(reservation_pool_t), intent(in) :: this
      integer(default_int) :: n

      n = this%cap
   end function pool_capacity

   subroutine pool_destroy(this)
      !! Release the pool.
      class(reservation_pool_t), intent(inout) :: this

      if (allocated(this%node)) deallocate (this%node)
      if (allocated(this%from)) deallocate (this%from)
      if (allocated(this%to)) deallocate (this%to)
      if (allocated(this%who)) deallocate (this%who)
      if (allocated(this%next)) deallocate (this%next)
      if (allocated(this%head)) deallocate (this%head)
      this%free_head = NO_ENTRY
      this%cap = 0_default_int
      this%live = 0_default_int
   end subroutine pool_destroy

end module core_reservations
