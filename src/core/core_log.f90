! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The event log and the determinism digest.
module core_log
   !! Every event that actually dispatches is folded into a 64-bit digest.
   !!
   !! That digest is the determinism contract. Two builds agree if and only if
   !! they produced the same sequence of events at the same sim times, and the
   !! cross-compiler CI job is a comparison of these sixteen hex characters and
   !! nothing else.
   !!
   !! `tick`, `kind`, `entity` and `payload` are folded; `seq` and `generation`
   !! are not. Those two are queue bookkeeping: a change that reorders how
   !! events are *scheduled* without changing what *happens* should not look
   !! like a divergence. What the simulation did is the four fields above.
   !!
   !! Folding goes through `pic_array_hash`'s 64-bit FNV-1a, whose byte stream
   !! is specified down to the byte and pinned by reference vectors, so the
   !! digest does not depend on the compiler's idea of how to lay out an
   !! integer.
   use core_kinds, only: tick_k, id_k, int16, int32, int64
   use core_event, only: event_t
   use pic_types, only: default_int
   use pic_array_hash, only: array_hash64_t, array_hash64_hex
   implicit none
   private

   public :: event_log_t

   type :: event_log_t
      !! Streaming digest plus the counters the batch report prints.
      private
      type(array_hash64_t) :: hasher
         !! Running digest over the dispatched event stream.
      integer(int64) :: n_events = 0_int64
         !! Events folded in so far.
      integer(int64) :: n_stale = 0_int64
         !! Events dropped as stale, counted but not folded.
      integer(tick_k) :: last_tick = 0_tick_k
         !! Sim time of the most recent folded event.
   contains
      procedure :: record => log_record
      procedure :: record_stale => log_record_stale
      procedure :: digest => log_digest
      procedure :: digest_hex => log_digest_hex
      procedure :: count => log_count
      procedure :: stale_count => log_stale_count
      procedure :: last_event_tick => log_last_event_tick
      procedure :: reset => log_reset
   end type event_log_t

contains

   subroutine log_record(this, event)
      !! Fold one dispatched event into the digest.
      class(event_log_t), intent(inout) :: this
      type(event_t), intent(in) :: event
         !! The event about to be dispatched.

      call this%hasher%update(event%tick)
      call this%hasher%update(event%kind)
      call this%hasher%update(event%entity)
      call this%hasher%update(event%payload)

      this%n_events = this%n_events + 1_int64
      this%last_tick = event%tick
   end subroutine log_record

   subroutine log_record_stale(this)
      !! Count a tombstoned event without folding it.
      !!
      !! Stale events are deliberately invisible to the digest: how many dead
      !! entries a cancellation left in the queue is an implementation detail,
      !! and two builds that cancel the same things must agree whether or not
      !! they leave the same litter behind.
      class(event_log_t), intent(inout) :: this

      this%n_stale = this%n_stale + 1_int64
   end subroutine log_record_stale

   function log_digest(this) result(digest)
      !! The digest so far.
      class(event_log_t), intent(in) :: this
      integer(int64) :: digest

      digest = this%hasher%digest()
   end function log_digest

   function log_digest_hex(this) result(text)
      !! The digest as sixteen lowercase hex characters.
      class(event_log_t), intent(in) :: this
      character(len=16) :: text

      text = array_hash64_hex(this%hasher%digest())
   end function log_digest_hex

   pure function log_count(this) result(n)
      !! Events folded in.
      class(event_log_t), intent(in) :: this
      integer(int64) :: n

      n = this%n_events
   end function log_count

   pure function log_stale_count(this) result(n)
      !! Events dropped as stale.
      class(event_log_t), intent(in) :: this
      integer(int64) :: n

      n = this%n_stale
   end function log_stale_count

   pure function log_last_event_tick(this) result(tick)
      !! Sim time of the most recent folded event.
      class(event_log_t), intent(in) :: this
      integer(tick_k) :: tick

      tick = this%last_tick
   end function log_last_event_tick

   subroutine log_reset(this)
      !! Start a fresh log.
      class(event_log_t), intent(inout) :: this

      call this%hasher%reset()
      this%n_events = 0_int64
      this%n_stale = 0_int64
      this%last_tick = 0_tick_k
   end subroutine log_reset

end module core_log
