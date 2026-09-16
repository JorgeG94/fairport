! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! The taxiway graph: integer compressed sparse row, and routing over it.
module core_graph
   !! Static topology, loaded once, never mutated during a session.
   !!
   !! pic ships `pic_csr` and `pic_graph`, with Dijkstra and A* already
   !! written, and fairport deliberately does not use them here: their edge
   !! weights are `real(dp)`. Route cost is taxi time, taxi time schedules
   !! events, and event ordering is the simulation, so the weights have to be
   !! the same integers everywhere. `pic_heap` underneath is doing the actual
   !! work, so what this module adds over pic is the integer arithmetic, not a
   !! second shortest-path implementation.
   use core_kinds, only: id_k, int8, int32, int64, NO_ID
   use pic_types, only: default_int, int_index_low
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_VALIDATION
   use pic_heap, only: heap_t
   use pic_sorting, only: sort_index
   implicit none
   private

   public :: taxi_graph_t
   public :: NODE_INTERSECTION, NODE_GATE, NODE_HOLD_SHORT
   public :: NODE_RUNWAY_THRESHOLD, NODE_RUNWAY_EXIT, NODE_DEICE_PAD
   public :: node_kind_name

   integer(int8), parameter :: NODE_INTERSECTION = 0_int8
   integer(int8), parameter :: NODE_GATE = 1_int8
   integer(int8), parameter :: NODE_HOLD_SHORT = 2_int8
   integer(int8), parameter :: NODE_RUNWAY_THRESHOLD = 3_int8
   integer(int8), parameter :: NODE_RUNWAY_EXIT = 4_int8
   integer(int8), parameter :: NODE_DEICE_PAD = 5_int8

   integer(int64), parameter :: COST_INFINITY = ishft(huge(0_int64), -2)
      !! Unreachable distance. Quartered so that adding an edge cost to it
      !! cannot overflow during relaxation. Shifted rather than divided,
      !! because an integer division of a constant is a warning worth keeping
      !! switched on everywhere else.

   type :: taxi_graph_t
      !! Compressed sparse row adjacency plus per-node attributes.
      !!
      !! `xadj`, `adjncy` and `edge_cost` are the standard CSR triple: the
      !! neighbours of node `n` are `adjncy(xadj(n) : xadj(n+1) - 1)` and their
      !! edge costs sit at the same offsets.
      integer(int32), allocatable :: xadj(:)
         !! Row offsets, size `n_nodes + 1`, one-based.
      integer(id_k), allocatable :: adjncy(:)
         !! Neighbour node of every stored edge.
      integer(int32), allocatable :: edge_cost(:)
         !! Taxi time in milliseconds, parallel to `adjncy`.
      integer(int8), allocatable :: kind(:)
         !! Node kind, one of the `NODE_*` codes.
      integer(int32), allocatable :: x_cm(:)
         !! Local-frame x coordinate in centimetres. Integer, because float32
         !! degrees resolve to about 0.4 m and the jitter is visible.
      integer(int32), allocatable :: y_cm(:)
         !! Local-frame y coordinate in centimetres.
      integer(int8), allocatable :: max_wake(:)
         !! Largest wake category that fits through the node.
      integer(int32) :: n_nodes = 0_int32
         !! Nodes in the graph.
      integer(int32) :: n_edges = 0_int32
         !! Directed edges in the graph.
   contains
      procedure :: build => graph_build
      procedure :: degree => graph_degree
      procedure :: neighbour => graph_neighbour
      procedure :: neighbour_cost => graph_neighbour_cost
      procedure :: shortest_path => graph_shortest_path
      procedure :: destroy => graph_destroy
   end type taxi_graph_t

contains

   pure function node_kind_name(kind) result(name)
      !! Human-readable node kind, for loader diagnostics.
      integer(int8), intent(in) :: kind
         !! One of the `NODE_*` codes.
      character(len=:), allocatable :: name

      select case (kind)
      case (NODE_INTERSECTION)
         name = "intersection"
      case (NODE_GATE)
         name = "gate"
      case (NODE_HOLD_SHORT)
         name = "hold_short"
      case (NODE_RUNWAY_THRESHOLD)
         name = "runway_threshold"
      case (NODE_RUNWAY_EXIT)
         name = "runway_exit"
      case (NODE_DEICE_PAD)
         name = "deice_pad"
      case default
         name = "unknown"
      end select
   end function node_kind_name

   subroutine graph_build(this, n_nodes, edge_from, edge_to, edge_cost, err)
      !! Build the CSR adjacency from an undirected edge list.
      !!
      !! Every edge is stored in both directions, so a taxiway can be used
      !! either way. Neighbours end up ascending within each row, which makes
      !! the traversal order a property of the topology rather than of the
      !! order the loader happened to read the file in.
      class(taxi_graph_t), intent(inout) :: this
      integer(int32), intent(in) :: n_nodes
         !! Nodes to build for; node attributes must already be sized.
      integer(id_k), intent(in) :: edge_from(:)
         !! One endpoint of every edge.
      integer(id_k), intent(in) :: edge_to(:)
         !! The other endpoint, same length as `edge_from`.
      integer(int32), intent(in) :: edge_cost(:)
         !! Taxi time in milliseconds, same length as `edge_from`.
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: n_given, i
      integer(int32), allocatable :: fill(:)
      integer(int32) :: node, slot
      integer :: status

      n_given = int(size(edge_from), default_int)
      if (int(size(edge_to), default_int) /= n_given .or. &
          int(size(edge_cost), default_int) /= n_given) then
         call error_raise(err, ERROR_VALIDATION, "graph_build: edge arrays differ in length")
         return
      end if
      if (n_nodes < 0_int32) then
         call error_raise(err, ERROR_VALIDATION, "graph_build: negative node count")
         return
      end if
      do i = 1, n_given
         if (edge_from(i) < 1_id_k .or. edge_from(i) > n_nodes .or. &
             edge_to(i) < 1_id_k .or. edge_to(i) > n_nodes) then
            call error_raise(err, ERROR_VALIDATION, "graph_build: edge endpoint outside 1:n_nodes")
            return
         end if
         if (edge_cost(i) < 0_int32) then
            call error_raise(err, ERROR_VALIDATION, "graph_build: negative edge cost")
            return
         end if
      end do

      this%n_nodes = n_nodes
      this%n_edges = int(2_default_int*n_given, int32)

      if (allocated(this%xadj)) deallocate (this%xadj)
      if (allocated(this%adjncy)) deallocate (this%adjncy)
      if (allocated(this%edge_cost)) deallocate (this%edge_cost)
      allocate (this%xadj(n_nodes + 1), this%adjncy(this%n_edges), &
                this%edge_cost(this%n_edges), fill(n_nodes), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "graph_build: allocation failed")
         return
      end if

      ! Counting sort by row: count degrees, prefix sum into xadj, then place.
      this%xadj = 0_int32
      do i = 1, n_given
         this%xadj(edge_from(i) + 1) = this%xadj(edge_from(i) + 1) + 1_int32
         this%xadj(edge_to(i) + 1) = this%xadj(edge_to(i) + 1) + 1_int32
      end do
      this%xadj(1) = 1_int32
      do node = 1_int32, n_nodes
         this%xadj(node + 1) = this%xadj(node + 1) + this%xadj(node)
      end do

      fill = this%xadj(1:n_nodes)
      do i = 1, n_given
         slot = fill(edge_from(i))
         this%adjncy(slot) = edge_to(i)
         this%edge_cost(slot) = edge_cost(i)
         fill(edge_from(i)) = slot + 1_int32

         slot = fill(edge_to(i))
         this%adjncy(slot) = edge_from(i)
         this%edge_cost(slot) = edge_cost(i)
         fill(edge_to(i)) = slot + 1_int32
      end do

      call sort_rows(this, err)
   end subroutine graph_build

   subroutine sort_rows(this, err)
      !! Put each row's neighbours in ascending node order.
      !!
      !! `pic_sorting`'s `sort_index` rather than a hand-rolled sort. Two arrays
      !! move together here -- the neighbour and its edge cost -- so what is
      !! needed is a permutation, not just an ordered array. `sort_index`
      !! returns exactly that: `order(k)` is the one-based position, in the row
      !! as it was read, of the entry that belongs at `k`.
      !!
      !! Stability matters even though a well-formed airport has no parallel
      !! edges. A file that declares the same edge twice with different costs
      !! must still load identically every time, rather than keeping whichever
      !! cost the sort happened to leave in front. `sort_index` is documented
      !! stable; the hand-rolled insertion sort this replaced was too.
      !!
      !! Not `pure`, because `sort_index` is not. `graph_build` is the only
      !! caller and is not pure either, so nothing is given up downstream.
      type(taxi_graph_t), intent(inout) :: this
      type(error_t), intent(inout), optional :: err

      integer(int32), allocatable :: keys(:), costs(:)
      integer(int_index_low), allocatable :: order(:)
      integer(int32) :: node, lo, hi, degree, widest
      integer :: status

      widest = 0_int32
      do node = 1_int32, this%n_nodes
         widest = max(widest, this%xadj(node + 1) - this%xadj(node))
      end do
      if (widest <= 1_int32) return

      ! One scratch buffer sized to the busiest intersection and reused for
      ! every row, rather than an allocation per node. Taxiway rows are a
      ! handful of entries, so the allocation would otherwise dominate.
      allocate (keys(widest), costs(widest), order(widest), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "sort_rows: scratch allocation failed")
         return
      end if

      do node = 1_int32, this%n_nodes
         lo = this%xadj(node)
         hi = this%xadj(node + 1) - 1_int32
         degree = hi - lo + 1_int32
         if (degree <= 1_int32) cycle

         keys(1:degree) = this%adjncy(lo:hi)
         costs(1:degree) = this%edge_cost(lo:hi)

         call sort_index(keys(1:degree), order(1:degree), err=err)
         if (present(err)) then
            if (err%has_error()) return
         end if

         this%adjncy(lo:hi) = keys(1:degree)
         this%edge_cost(lo:hi) = costs(order(1:degree))
      end do
   end subroutine sort_rows

   pure function graph_degree(this, node) result(degree)
      !! Number of edges leaving `node`.
      class(taxi_graph_t), intent(in) :: this
      integer(id_k), intent(in) :: node
         !! Node to query, in `1:n_nodes`.
      integer(int32) :: degree

      degree = this%xadj(node + 1) - this%xadj(node)
   end function graph_degree

   pure function graph_neighbour(this, node, which) result(neighbour)
      !! The `which`-th neighbour of `node`, one-based.
      class(taxi_graph_t), intent(in) :: this
      integer(id_k), intent(in) :: node
         !! Node to query.
      integer(int32), intent(in) :: which
         !! Which neighbour, in `1:degree(node)`.
      integer(id_k) :: neighbour

      neighbour = this%adjncy(this%xadj(node) + which - 1_int32)
   end function graph_neighbour

   pure function graph_neighbour_cost(this, node, which) result(cost)
      !! Taxi time in milliseconds to the `which`-th neighbour of `node`.
      class(taxi_graph_t), intent(in) :: this
      integer(id_k), intent(in) :: node
         !! Node to query.
      integer(int32), intent(in) :: which
         !! Which neighbour, in `1:degree(node)`.
      integer(int32) :: cost

      cost = this%edge_cost(this%xadj(node) + which - 1_int32)
   end function graph_neighbour_cost

   subroutine graph_shortest_path(this, source, target, route, route_len, cost, err)
      !! Cheapest route from `source` to `target`, in whole milliseconds.
      !!
      !! Dijkstra with lazy deletion over `pic_heap`. Distances are
      !! `integer(int64)` milliseconds, so the arithmetic is exact and two
      !! compilers cannot disagree about which of two routes is shorter. Ties
      !! are broken by the heap's FIFO rule on equal keys, which makes the
      !! chosen route a function of the graph and nothing else.
      class(taxi_graph_t), intent(in) :: this
      integer(id_k), intent(in) :: source
         !! Start node.
      integer(id_k), intent(in) :: target
         !! Goal node.
      integer(id_k), intent(out) :: route(:)
         !! Nodes from `source` to `target` inclusive, filled in `1:route_len`.
      integer(int32), intent(out) :: route_len
         !! Nodes written, or zero when the target is unreachable.
      integer(int64), intent(out) :: cost
         !! Total taxi time in milliseconds, or -1 when unreachable.
      type(error_t), intent(inout), optional :: err

      type(heap_t) :: queue
      integer(int64), allocatable :: dist(:)
      integer(id_k), allocatable :: prev(:)
      logical, allocatable :: settled(:)
      integer(int64) :: key, candidate
      integer(int32) :: payload, which, degree, hops
      integer(id_k) :: node, next_node
      integer :: status

      route_len = 0_int32
      cost = -1_int64

      if (source < 1_id_k .or. source > this%n_nodes .or. &
          target < 1_id_k .or. target > this%n_nodes) then
         call error_raise(err, ERROR_VALIDATION, "graph_shortest_path: endpoint outside 1:n_nodes")
         return
      end if

      allocate (dist(this%n_nodes), prev(this%n_nodes), settled(this%n_nodes), stat=status)
      if (status /= 0) then
         call error_raise(err, ERROR_ALLOC, "graph_shortest_path: allocation failed")
         return
      end if
      dist = COST_INFINITY
      prev = NO_ID
      settled = .false.

      call queue%init(int(this%n_nodes, default_int))
      dist(source) = 0_int64
      call queue%push(0_int64, int(source, int32))

      do while (.not. queue%is_empty())
         call queue%pop(key, payload)
         node = int(payload, id_k)
         if (settled(node)) cycle
         settled(node) = .true.
         if (node == target) exit

         degree = this%degree(node)
         do which = 1_int32, degree
            next_node = this%neighbour(node, which)
            if (settled(next_node)) cycle
            candidate = dist(node) + int(this%neighbour_cost(node, which), int64)
            if (candidate < dist(next_node)) then
               dist(next_node) = candidate
               prev(next_node) = node
               call queue%push(candidate, int(next_node, int32))
            end if
         end do
      end do

      call queue%destroy()

      if (.not. settled(target)) return

      ! Walk the predecessors back, then reverse in place.
      hops = 0_int32
      node = target
      do
         hops = hops + 1_int32
         if (hops > int(size(route), int32)) then
            call error_raise(err, ERROR_VALIDATION, "graph_shortest_path: route does not fit the caller's buffer")
            route_len = 0_int32
            return
         end if
         route(hops) = node
         if (node == source) exit
         node = prev(node)
      end do

      route(1:hops) = route(hops:1:-1)
      route_len = hops
      cost = dist(target)
   end subroutine graph_shortest_path

   subroutine graph_destroy(this)
      !! Release every array.
      class(taxi_graph_t), intent(inout) :: this

      if (allocated(this%xadj)) deallocate (this%xadj)
      if (allocated(this%adjncy)) deallocate (this%adjncy)
      if (allocated(this%edge_cost)) deallocate (this%edge_cost)
      if (allocated(this%kind)) deallocate (this%kind)
      if (allocated(this%x_cm)) deallocate (this%x_cm)
      if (allocated(this%y_cm)) deallocate (this%y_cm)
      if (allocated(this%max_wake)) deallocate (this%max_wake)
      this%n_nodes = 0_int32
      this%n_edges = 0_int32
   end subroutine graph_destroy

end module core_graph
