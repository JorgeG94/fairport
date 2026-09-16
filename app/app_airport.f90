! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Loading an airport from a text file.
module app_airport
   !! Plain text, one directive per line, tokenised with `pic_tokenizer`.
   !!
   !! The design document specifies TOML here and `toml-f` to read it. That is
   !! still the plan, and it arrives with the milestone 1 schedule loader,
   !! where nested tables start to earn their keep. Milestone 0 needs fifteen
   !! nodes and six stands, so this format keeps the build free of a fetched
   !! dependency and keeps the loader short enough to read in one sitting.
   !!
   !! Nodes are numbered implicitly, in the order they appear. There is no
   !! name-to-id map to get wrong, and two files with the same nodes in the
   !! same order describe the same airport.
   use core_sim, only: sim_t, CALLSIGN_LEN, &
                       NODE_INTERSECTION, NODE_GATE, NODE_HOLD_SHORT, &
                       NODE_RUNWAY_THRESHOLD, NODE_RUNWAY_EXIT, NODE_DEICE_PAD, &
                       WAKE_LIGHT, WAKE_MEDIUM, WAKE_HEAVY, WAKE_SUPER
   use pic_types, only: default_int, int8, int32, int64
   use pic_error, only: error_t, error_raise, ERROR_IO, ERROR_PARSE, ERROR_VALIDATION
   use pic_string_type, only: string_type, char
   use pic_tokenizer, only: tokenize, parse_int
   use pic_vector, only: vector_int32_t
   use app_text, only: int_text
   implicit none
   private

   public :: load_airport

   integer, parameter :: MAX_LINE = 512
      !! Longest input line the loader accepts.

contains

   subroutine load_airport(sim, path, max_aircraft, err)
      !! Read an airport file into `sim`'s world.
      !!
      !! Two passes would need the file twice; instead every list is
      !! accumulated in a `pic_vector` whose final length nobody has to know in
      !! advance, and `take` hands the storage over exactly sized at the end.
      !! That is the load-time idiom pic's vector exists for.
      type(sim_t), intent(inout) :: sim
      character(len=*), intent(in) :: path
         !! Path to the airport file.
      integer(default_int), intent(in) :: max_aircraft
         !! Aircraft slots to reserve; the scenario knows this, the airport
         !! does not.
      type(error_t), intent(inout), optional :: err

      type(vector_int32_t) :: node_kind, node_x, node_y, node_wake
      type(vector_int32_t) :: edge_from, edge_to, edge_cost
      type(vector_int32_t) :: gate_node, gate_wake
      type(vector_int32_t) :: runway_threshold, runway_exit
      character(len=CALLSIGN_LEN), allocatable :: gate_label(:), runway_label(:)
      character(len=MAX_LINE) :: line
      type(string_type), allocatable :: tokens(:)
      integer :: unit, status
      integer(default_int) :: line_no, n_tokens
      integer(int32) :: n_nodes, n_gates, n_runways

      allocate (gate_label(0), runway_label(0))

      open (newunit=unit, file=path, status="old", action="read", iostat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_IO, "load_airport: cannot open "//trim(path))
         return
      end if

      line_no = 0_default_int
      do
         read (unit, "(a)", iostat=status) line
         if (status /= 0) exit
         line_no = line_no + 1_default_int

         call strip_comment(line)
         tokens = tokenize(trim(line))
         n_tokens = int(size(tokens), default_int)
         if (n_tokens == 0_default_int) cycle

         select case (char(tokens(1)))
         case ("name")
            ! Decorative. Kept so that a file names itself.
         case ("node")
            call read_node(tokens, node_kind, node_x, node_y, node_wake, line_no, err)
         case ("edge")
            call read_edge(tokens, edge_from, edge_to, edge_cost, line_no, err)
         case ("gate")
            call read_gate(tokens, gate_node, gate_wake, gate_label, line_no, err)
         case ("runway")
            call read_runway(tokens, runway_threshold, runway_exit, runway_label, line_no, err)
         case default
            call error_raise(err, ERROR_PARSE, "load_airport: unknown directive '"// &
                             char(tokens(1))//"' on line "//int_text(int(line_no, int64)))
         end select

         if (present(err)) then
            if (err%has_error()) then
               close (unit)
               return
            end if
         end if
      end do
      close (unit)

      n_nodes = int(node_kind%size(), int32)
      n_gates = int(gate_node%size(), int32)
      n_runways = int(runway_threshold%size(), int32)

      if (n_nodes == 0_int32) then
         call error_raise(err, ERROR_VALIDATION, "load_airport: no nodes in "//trim(path))
         return
      end if
      if (n_runways == 0_int32) then
         call error_raise(err, ERROR_VALIDATION, "load_airport: no runways in "//trim(path))
         return
      end if

      call sim%world%reserve(max_aircraft, n_gates, n_runways, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call fill_graph(sim, node_kind, node_x, node_y, node_wake, &
                      edge_from, edge_to, edge_cost, n_nodes, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call fill_stands(sim, gate_node, gate_wake, gate_label, &
                       runway_threshold, runway_exit, runway_label, err)
   end subroutine load_airport

   subroutine fill_graph(sim, node_kind, node_x, node_y, node_wake, &
                         edge_from, edge_to, edge_cost, n_nodes, err)
      !! Move the accumulated node and edge lists into the world's graph.
      type(sim_t), intent(inout) :: sim
      type(vector_int32_t), intent(inout) :: node_kind, node_x, node_y, node_wake
      type(vector_int32_t), intent(inout) :: edge_from, edge_to, edge_cost
      integer(int32), intent(in) :: n_nodes
         !! Nodes read.
      type(error_t), intent(inout), optional :: err

      integer(int32), allocatable :: kinds(:), wakes(:), from(:), to(:), cost(:)
      integer :: status

      call node_kind%take(kinds, err)
      call node_wake%take(wakes, err)
      call node_x%take(sim%world%graph%x_cm, err)
      call node_y%take(sim%world%graph%y_cm, err)
      call edge_from%take(from, err)
      call edge_to%take(to, err)
      call edge_cost%take(cost, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      allocate (sim%world%graph%kind(n_nodes), sim%world%graph%max_wake(n_nodes), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_VALIDATION, "load_airport: node attribute allocation failed")
         return
      end if
      sim%world%graph%kind = int(kinds, int8)
      sim%world%graph%max_wake = int(wakes, int8)

      call sim%world%graph%build(n_nodes, from, to, cost, err)
   end subroutine fill_graph

   subroutine fill_stands(sim, gate_node, gate_wake, gate_label, &
                          runway_threshold, runway_exit, runway_label, err)
      !! Move the accumulated stand and runway lists into the world.
      type(sim_t), intent(inout) :: sim
      type(vector_int32_t), intent(inout) :: gate_node, gate_wake
      character(len=CALLSIGN_LEN), intent(in) :: gate_label(:)
         !! Stand labels, in declaration order.
      type(vector_int32_t), intent(inout) :: runway_threshold, runway_exit
      character(len=CALLSIGN_LEN), intent(in) :: runway_label(:)
         !! Runway designators, in declaration order.
      type(error_t), intent(inout), optional :: err

      integer(int32), allocatable :: nodes(:), wakes(:), thresholds(:), exits(:)
      integer(default_int) :: i

      call gate_node%take(nodes, err)
      call gate_wake%take(wakes, err)
      call runway_threshold%take(thresholds, err)
      call runway_exit%take(exits, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      do i = 1_default_int, int(size(nodes), default_int)
         sim%world%gate_node(i) = nodes(i)
         sim%world%gate_max_wake(i) = wakes(i)
         sim%world%gate_name(i) = gate_label(i)
      end do

      do i = 1_default_int, int(size(thresholds), default_int)
         sim%world%runway_threshold(i) = thresholds(i)
         sim%world%runway_exit(i) = exits(i)
         sim%world%runway_name(i) = runway_label(i)
      end do
   end subroutine fill_stands

   subroutine read_node(tokens, kinds, xs, ys, wakes, line_no, err)
      !! `node <kind> <x_cm> <y_cm> <max_wake>`
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      type(vector_int32_t), intent(inout) :: kinds, xs, ys, wakes
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: x, y

      if (size(tokens) /= 5) then
         call error_raise(err, ERROR_PARSE, "load_airport: node needs 4 fields on line "//int_text(int(line_no, int64)))
         return
      end if

      call parse_int_field(tokens(3), x, line_no, err)
      call parse_int_field(tokens(4), y, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call kinds%push_back(node_kind_code(char(tokens(2)), line_no, err), err)
      call xs%push_back(x, err)
      call ys%push_back(y, err)
      call wakes%push_back(wake_code(char(tokens(5)), line_no, err), err)
   end subroutine read_node

   subroutine read_edge(tokens, from, to, cost, line_no, err)
      !! `edge <from_node> <to_node> <cost_ms>`
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      type(vector_int32_t), intent(inout) :: from, to, cost
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: a, b, c

      if (size(tokens) /= 4) then
         call error_raise(err, ERROR_PARSE, "load_airport: edge needs 3 fields on line "//int_text(int(line_no, int64)))
         return
      end if

      call parse_int_field(tokens(2), a, line_no, err)
      call parse_int_field(tokens(3), b, line_no, err)
      call parse_int_field(tokens(4), c, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call from%push_back(a, err)
      call to%push_back(b, err)
      call cost%push_back(c, err)
   end subroutine read_edge

   subroutine read_gate(tokens, nodes, wakes, labels, line_no, err)
      !! `gate <label> <node> <max_wake>`
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      type(vector_int32_t), intent(inout) :: nodes, wakes
      character(len=CALLSIGN_LEN), allocatable, intent(inout) :: labels(:)
         !! Stand labels, appended to.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: node

      if (size(tokens) /= 4) then
         call error_raise(err, ERROR_PARSE, "load_airport: gate needs 3 fields on line "//int_text(int(line_no, int64)))
         return
      end if

      call parse_int_field(tokens(3), node, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call nodes%push_back(node, err)
      call wakes%push_back(wake_code(char(tokens(4)), line_no, err), err)
      call append_label(labels, char(tokens(2)))
   end subroutine read_gate

   subroutine read_runway(tokens, thresholds, exits, labels, line_no, err)
      !! `runway <label> <threshold_node> <exit_node>`
      type(string_type), intent(in) :: tokens(:)
         !! The tokenised line.
      type(vector_int32_t), intent(inout) :: thresholds, exits
      character(len=CALLSIGN_LEN), allocatable, intent(inout) :: labels(:)
         !! Runway designators, appended to.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      integer(int32) :: threshold, exit_node

      if (size(tokens) /= 4) then
         call error_raise(err, ERROR_PARSE, "load_airport: runway needs 3 fields on line "//int_text(int(line_no, int64)))
         return
      end if

      call parse_int_field(tokens(3), threshold, line_no, err)
      call parse_int_field(tokens(4), exit_node, line_no, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      call thresholds%push_back(threshold, err)
      call exits%push_back(exit_node, err)
      call append_label(labels, char(tokens(2)))
   end subroutine read_runway

   subroutine append_label(labels, text)
      !! Grow a label array by one.
      character(len=CALLSIGN_LEN), allocatable, intent(inout) :: labels(:)
         !! Array to extend.
      character(len=*), intent(in) :: text
         !! Label to append; truncated to `CALLSIGN_LEN`.

      character(len=CALLSIGN_LEN), allocatable :: bigger(:)
      integer(default_int) :: n

      n = int(size(labels), default_int)
      allocate (bigger(n + 1_default_int))
      if (n > 0_default_int) bigger(1:n) = labels
      bigger(n + 1_default_int) = text
      call move_alloc(bigger, labels)
   end subroutine append_label

   subroutine parse_int_field(token, value, line_no, err)
      !! Parse one integer token, reporting where it failed.
      type(string_type), intent(in) :: token
         !! Token to convert.
      integer(int32), intent(out) :: value
         !! The parsed value, zero on failure.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err

      type(error_t) :: parse_err

      call parse_int(char(token), value, parse_err)
      if (parse_err%has_error()) then
         value = 0_int32
         call error_raise(err, ERROR_PARSE, "load_airport: '"//char(token)// &
                          "' is not an integer, on line "//int_text(int(line_no, int64)))
      end if
   end subroutine parse_int_field

   function node_kind_code(text, line_no, err) result(code)
      !! Map a node kind name to its code.
      character(len=*), intent(in) :: text
         !! Name as written in the file.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err
      integer(int32) :: code

      select case (text)
      case ("intersection")
         code = int(NODE_INTERSECTION, int32)
      case ("gate")
         code = int(NODE_GATE, int32)
      case ("hold_short")
         code = int(NODE_HOLD_SHORT, int32)
      case ("runway_threshold")
         code = int(NODE_RUNWAY_THRESHOLD, int32)
      case ("runway_exit")
         code = int(NODE_RUNWAY_EXIT, int32)
      case ("deice_pad")
         code = int(NODE_DEICE_PAD, int32)
      case default
         code = int(NODE_INTERSECTION, int32)
         call error_raise(err, ERROR_PARSE, "load_airport: unknown node kind '"//text// &
                          "' on line "//int_text(int(line_no, int64)))
      end select
   end function node_kind_code

   function wake_code(text, line_no, err) result(code)
      !! Map a wake category letter to its code.
      character(len=*), intent(in) :: text
         !! One of L, M, H, S.
      integer(default_int), intent(in) :: line_no
         !! Line number, for diagnostics.
      type(error_t), intent(inout), optional :: err
      integer(int32) :: code

      select case (text)
      case ("L", "l")
         code = WAKE_LIGHT
      case ("M", "m")
         code = WAKE_MEDIUM
      case ("H", "h")
         code = WAKE_HEAVY
      case ("S", "s")
         code = WAKE_SUPER
      case default
         code = WAKE_MEDIUM
         call error_raise(err, ERROR_PARSE, "load_airport: unknown wake category '"//text// &
                          "' on line "//int_text(int(line_no, int64)))
      end select
   end function wake_code

   pure subroutine strip_comment(line)
      !! Blank everything from the first `#` onwards.
      character(len=*), intent(inout) :: line
         !! Line to strip, in place.

      integer(default_int) :: hash

      hash = index(line, "#")
      if (hash > 0_default_int) line(hash:) = " "
   end subroutine strip_comment

end module app_airport
