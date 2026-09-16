! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for player commands and the command log.
module test_core_command
   !! The other half of the invariant.
   !!
   !! `(seed, command log)` reproduces a session. Milestone 0 had the seed and
   !! a single hard-wired entry point; these tests cover the log, the round
   !! trip through it, and the property that makes it worth having -- that the
   !! same commands replayed produce the same simulation, and different ones do
   !! not.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_command, only: command_t, command_log_t, command_name, &
                           command_kind_from_name, command_arg_count, &
                           is_command_kind, COMMAND_ID_MIN, COMMAND_ID_MAX, N_COMMANDS
   use core_event_kinds, only: MAX_EVENT_KIND, event_name, &
                               K_CMD_ASSIGN_GATE, K_TOUCHDOWN, K_ONBLOCKS, K_BAGSTOLEN
   use core_kinds, only: tick_k, id_k, int16, int32, int64, NO_ID
   use pic_error, only: error_t
   use pic_types, only: default_int
   implicit none
   private

   public :: collect_core_command_tests

contains

   subroutine collect_core_command_tests(testsuite)
      !! Register the command tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("names_round_trip", test_names_round_trip), &
                  new_unittest("unknown_name_is_zero", test_unknown_name), &
                  new_unittest("command_ids_are_in_their_range", test_ids_in_range), &
                  new_unittest("command_ids_do_not_collide_with_events", test_no_collision), &
                  new_unittest("log_records_in_order", test_log_order), &
                  new_unittest("log_round_trips_every_field", test_log_round_trip), &
                  new_unittest("log_rejects_a_non_command", test_log_rejects), &
                  new_unittest("log_rejects_a_bad_index", test_log_bad_index), &
                  new_unittest("log_grows_past_capacity", test_log_growth), &
                  new_unittest("digest_follows_the_log", test_digest_follows), &
                  new_unittest("digest_sees_submission_time", test_digest_sees_time) &
                  ]
   end subroutine collect_core_command_tests

   subroutine test_names_round_trip(error)
      !! Every declared command parses from its own name.
      !!
      !! Both directions come from one list, so this cannot drift -- but it is
      !! the list itself that a scenario writer types, and a command that
      !! renders as something it cannot parse back would make a saved log
      !! unreadable by the simulator that wrote it.
      type(error_type), allocatable, intent(out) :: error

      integer(int16) :: kind
      integer(int32) :: found

      found = 0_int32
      do kind = COMMAND_ID_MIN, COMMAND_ID_MAX
         if (command_name(kind) == "unknown") cycle
         found = found + 1_int32
         call check(error, command_kind_from_name(command_name(kind)) == kind, &
                    "a command name did not parse back to its own kind")
         if (allocated(error)) return
      end do

      call check(error, found == int(N_COMMANDS, int32), &
                 "the number of named commands does not match N_COMMANDS")
   end subroutine test_names_round_trip

   subroutine test_unknown_name(error)
      !! An unrecognised keyword is zero, not a wrong command.
      type(error_type), allocatable, intent(out) :: error

      call check(error, command_kind_from_name("not_a_command") == 0_int16, "unknown keyword")
      if (allocated(error)) return
      call check(error, command_kind_from_name("") == 0_int16, "empty keyword")
      if (allocated(error)) return
      call check(error, command_kind_from_name("ASSIGN_GATE") == 0_int16, &
                 "keywords are case sensitive, so this should not match")
   end subroutine test_unknown_name

   subroutine test_ids_in_range(error)
      !! Commands live inside their reserved band, and `is_command_kind` agrees.
      type(error_type), allocatable, intent(out) :: error

      call check(error, is_command_kind(K_CMD_ASSIGN_GATE), "assign_gate is a command")
      if (allocated(error)) return
      call check(error,.not. is_command_kind(K_TOUCHDOWN), "touchdown is not a command")
      if (allocated(error)) return
      call check(error,.not. is_command_kind(K_BAGSTOLEN), "a landside event is not a command")
      if (allocated(error)) return
      call check(error, K_CMD_ASSIGN_GATE >= COMMAND_ID_MIN .and. &
                 K_CMD_ASSIGN_GATE <= COMMAND_ID_MAX, "assign_gate is outside the reserved band")
      if (allocated(error)) return
      call check(error, command_arg_count(K_CMD_ASSIGN_GATE) == 2_int32, "assign_gate takes two")
   end subroutine test_ids_in_range

   subroutine test_no_collision(error)
      !! No non-command event uses an identifier in the command band.
      !!
      !! The two lists are numbered independently -- events in
      !! `core_event_kinds.fypp`, commands in `commands.fypp` -- and nothing in
      !! fypp stops someone adding event 61 next to command 61. This is the
      !! check that would catch it, and it is why the bus can dispatch both
      !! through one table.
      type(error_type), allocatable, intent(out) :: error

      integer(int16) :: kind

      do kind = COMMAND_ID_MIN, COMMAND_ID_MAX
         if (event_name(kind) == "Unknown") cycle
         ! Every known name in this band must be a command, which the generated
         ! `event_name` spells with a `Cmd` prefix.
         call check(error, command_name(kind) /= "unknown", &
                    "an event kind in the command band is not a command")
         if (allocated(error)) return
      end do

      ! Mixed-kind integer comparison is fine in Fortran, so no conversion.
      call check(error, MAX_EVENT_KIND <= COMMAND_ID_MAX, &
                 "an identifier was declared past the end of the command band")
   end subroutine test_no_collision

   subroutine test_log_order(error)
      !! The log preserves issue order, which is what a replay follows.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: log
      type(command_t) :: command, back
      type(error_t) :: err
      integer(int64) :: first, second
      integer(tick_k) :: at

      command%kind = K_CMD_ASSIGN_GATE
      command%a = 1_id_k
      command%b = 3_id_k
      call log%append(1000_tick_k, command, first, err)

      command%a = 2_id_k
      command%b = 4_id_k
      call log%append(2000_tick_k, command, second, err)

      call check(error,.not. err%has_error(), "append reported an error")
      if (allocated(error)) return
      call check(error, first == 1_int64 .and. second == 2_int64, "indices are not one-based and ascending")
      if (allocated(error)) return
      call check(error, log%size() == 2_default_int, "size")
      if (allocated(error)) return

      call log%get(first, at, back, err)
      call check(error, back%a == 1_id_k .and. at == 1000_tick_k, "the first command came back wrong")

      call log%destroy()
   end subroutine test_log_order

   subroutine test_log_round_trip(error)
      !! Every field of a command survives the log.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: log
      type(command_t) :: command, back
      type(error_t) :: err
      integer(int64) :: index
      integer(tick_k) :: at

      command%kind = K_CMD_ASSIGN_GATE
      command%a = 7_id_k
      command%b = 11_id_k
      command%value = -1234_int64
      call log%append(98765_tick_k, command, index, err)
      call log%get(index, at, back, err)

      call check(error,.not. err%has_error(), "round trip reported an error")
      if (allocated(error)) return
      call check(error, at == 98765_tick_k, "submission time")
      if (allocated(error)) return
      call check(error, back%kind == command%kind, "kind")
      if (allocated(error)) return
      call check(error, back%a == command%a, "subject a")
      if (allocated(error)) return
      call check(error, back%b == command%b, "subject b")
      if (allocated(error)) return
      call check(error, back%value == command%value, "value, which is negative")

      call log%destroy()
   end subroutine test_log_round_trip

   subroutine test_log_rejects(error)
      !! Only command kinds go in the command log.
      !!
      !! Otherwise an ordinary event could be recorded as player input, and a
      !! replay would issue it as a command.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: log
      type(command_t) :: command
      type(error_t) :: err
      integer(int64) :: index

      command%kind = K_ONBLOCKS
      command%a = 1_id_k
      call log%append(0_tick_k, command, index, err)

      call check(error, err%has_error(), "a non-command was accepted into the log")
      if (allocated(error)) return
      call check(error, index == 0_int64, "a rejected append returned an index")
      if (allocated(error)) return
      call check(error, log%size() == 0_default_int, "a rejected append grew the log")

      call log%destroy()
   end subroutine test_log_rejects

   subroutine test_log_bad_index(error)
      !! Reading outside the log is an error, not a wrong command.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: log
      type(command_t) :: command, back
      type(error_t) :: err, fault
      integer(int64) :: index
      integer(tick_k) :: at

      command%kind = K_CMD_ASSIGN_GATE
      command%a = 1_id_k
      call log%append(0_tick_k, command, index, err)

      call log%get(0_int64, at, back, fault)
      call check(error, fault%has_error(), "index zero should be rejected")
      if (allocated(error)) return

      call log%get(99_int64, at, back, fault)
      call check(error, fault%has_error(), "an index past the end should be rejected")
      if (allocated(error)) return
      call check(error, back%a == NO_ID, "a rejected read returned a subject")

      call log%destroy()
   end subroutine test_log_bad_index

   subroutine test_log_growth(error)
      !! The log grows without losing or reordering anything.
      !!
      !! It is the one thing in `core/` that grows during a run, because a
      !! session's length is not known when it starts.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: log
      type(command_t) :: command, back
      type(error_t) :: err
      integer(int64) :: index
      integer(tick_k) :: at
      integer(int32) :: i
      integer(int32), parameter :: N = 500_int32

      command%kind = K_CMD_ASSIGN_GATE
      do i = 1_int32, N
         command%a = int(i, id_k)
         call log%append(int(i, tick_k), command, index, err)
      end do

      call check(error,.not. err%has_error(), "growth reported an error")
      if (allocated(error)) return
      call check(error, log%size() == int(N, default_int), "size after growth")
      if (allocated(error)) return

      do i = 1_int32, N
         call log%get(int(i, int64), at, back, err)
         call check(error, back%a == int(i, id_k) .and. at == int(i, tick_k), &
                    "a command moved during growth")
         if (allocated(error)) return
      end do

      call log%destroy()
   end subroutine test_log_growth

   subroutine test_digest_follows(error)
      !! Two logs agree only when they hold the same commands.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: first, second
      type(command_t) :: command
      type(error_t) :: err
      integer(int64) :: index

      command%kind = K_CMD_ASSIGN_GATE
      command%a = 1_id_k
      command%b = 3_id_k

      call first%append(500_tick_k, command, index, err)
      call second%append(500_tick_k, command, index, err)
      call check(error, first%digest() == second%digest(), "identical logs disagree")
      if (allocated(error)) return

      command%b = 4_id_k
      call second%append(600_tick_k, command, index, err)
      call check(error, first%digest() /= second%digest(), "an extra command changed nothing")
      if (allocated(error)) return

      call check(error, len(first%digest_hex()) == 16, "hex digest width")

      call first%destroy()
      call second%destroy()
   end subroutine test_digest_follows

   subroutine test_digest_sees_time(error)
      !! The same command at a different time is a different session.
      !!
      !! A digest over the commands alone would call these identical, and they
      !! are not: when a stand was claimed decides who else could have had it.
      type(error_type), allocatable, intent(out) :: error

      type(command_log_t) :: early, late
      type(command_t) :: command
      type(error_t) :: err
      integer(int64) :: index

      command%kind = K_CMD_ASSIGN_GATE
      command%a = 1_id_k
      command%b = 3_id_k

      call early%append(1000_tick_k, command, index, err)
      call late%append(2000_tick_k, command, index, err)

      call check(error, early%digest() /= late%digest(), &
                 "submission time did not reach the digest")

      call early%destroy()
      call late%destroy()
   end subroutine test_digest_sees_time

end module test_core_command
