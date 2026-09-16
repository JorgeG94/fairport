! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
!! Tests for the taxiway graph and integer routing.
module test_core_graph
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use core_graph, only: taxi_graph_t
   use core_aircraft, only: AIRCRAFT_ROUTE_ROWS
   use core_kinds, only: id_k, int32, int64
   use pic_error, only: error_t
   implicit none
   private

   public :: collect_core_graph_tests

contains

   subroutine collect_core_graph_tests(testsuite)
      !! Register the graph tests.
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("build_csr_layout", test_build_csr_layout), &
                  new_unittest("neighbours_are_sorted", test_neighbours_sorted), &
                  new_unittest("costs_follow_their_neighbours", test_costs_follow), &
                  new_unittest("shortest_path_direct", test_shortest_path_direct), &
                  new_unittest("shortest_path_prefers_cheaper", test_prefers_cheaper), &
                  new_unittest("unreachable_is_not_an_error", test_unreachable), &
                  new_unittest("bad_endpoint_is_an_error", test_bad_endpoint), &
                  new_unittest("edge_order_does_not_matter", test_edge_order) &
                  ]
   end subroutine collect_core_graph_tests

   subroutine build_line(graph, err)
      !! A four-node line: 1 -10- 2 -20- 3 -30- 4.
      type(taxi_graph_t), intent(inout) :: graph
      type(error_t), intent(inout) :: err

      call graph%build(4_int32, [1_id_k, 2_id_k, 3_id_k], [2_id_k, 3_id_k, 4_id_k], &
                       [10_int32, 20_int32, 30_int32], err)
   end subroutine build_line

   subroutine test_build_csr_layout(error)
      !! Degrees and edge count come out as the topology says.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err

      call build_line(graph, err)
      call check(error,.not. err%has_error(), "build reported an error")
      if (allocated(error)) return

      call check(error, graph%n_nodes == 4_int32, "node count")
      if (allocated(error)) return
      ! Three undirected edges are six directed ones.
      call check(error, graph%n_edges == 6_int32, "edge count")
      if (allocated(error)) return

      call check(error, graph%degree(1_id_k) == 1_int32, "degree of an end node")
      if (allocated(error)) return
      call check(error, graph%degree(2_id_k) == 2_int32, "degree of a middle node")
   end subroutine test_build_csr_layout

   subroutine test_neighbours_sorted(error)
      !! Rows are ascending, whatever order the edges arrived in.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err
      integer(int32) :: which

      ! Node 1 joined to 4, 2 and 3, declared worst-first.
      call graph%build(4_int32, [1_id_k, 1_id_k, 1_id_k], [4_id_k, 3_id_k, 2_id_k], &
                       [10_int32, 10_int32, 10_int32], err)
      call check(error,.not. err%has_error(), "build reported an error")
      if (allocated(error)) return

      do which = 1_int32, graph%degree(1_id_k) - 1_int32
         call check(error, graph%neighbour(1_id_k, which) < graph%neighbour(1_id_k, which + 1_int32), &
                    "neighbours are not ascending")
         if (allocated(error)) return
      end do
   end subroutine test_neighbours_sorted

   subroutine test_costs_follow(error)
      !! Sorting a row must carry each edge cost with its neighbour.
      !!
      !! `sort_rows` reorders two parallel arrays through a permutation from
      !! `pic_sorting`'s `sort_index`. Checking only that the neighbours came
      !! out ascending would pass just as happily if the costs had been left
      !! where they were, which would silently reprice every taxiway in the
      !! airport and show up much later as a wrong route.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err
      integer(int32) :: which

      ! Node 1 joins 4, 3 and 2, declared worst-first, each with a cost that
      ! names its far end: node N costs N * 100.
      call graph%build(4_int32, [1_id_k, 1_id_k, 1_id_k], [4_id_k, 3_id_k, 2_id_k], &
                       [400_int32, 300_int32, 200_int32], err)
      call check(error,.not. err%has_error(), "build reported an error")
      if (allocated(error)) return

      call check(error, graph%degree(1_id_k) == 3_int32, "degree")
      if (allocated(error)) return

      do which = 1_int32, 3_int32
         call check(error, &
                    graph%neighbour_cost(1_id_k, which) == 100_int32*int(graph%neighbour(1_id_k, which), int32), &
                    "an edge cost was left behind when its neighbour moved")
         if (allocated(error)) return
      end do

      ! And the same from the other end, where the row was already in order.
      call check(error, graph%neighbour_cost(4_id_k, 1_int32) == 400_int32, &
                 "a single-entry row was disturbed")
   end subroutine test_costs_follow

   subroutine test_shortest_path_direct(error)
      !! End to end along the line, with the cost summed exactly.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err
      integer(id_k) :: route(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: route_len
      integer(int64) :: cost

      call build_line(graph, err)
      call graph%shortest_path(1_id_k, 4_id_k, route, route_len, cost, err)
      call check(error,.not. err%has_error(), "shortest_path reported an error")
      if (allocated(error)) return

      call check(error, route_len == 4_int32, "route length")
      if (allocated(error)) return
      call check(error, cost == 60_int64, "route cost")
      if (allocated(error)) return
      call check(error, route(1) == 1_id_k, "route starts at the source")
      if (allocated(error)) return
      call check(error, route(4) == 4_id_k, "route ends at the target")
   end subroutine test_shortest_path_direct

   subroutine test_prefers_cheaper(error)
      !! Given two ways round, the cheaper one wins on taxi time, not hops.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err
      integer(id_k) :: route(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: route_len
      integer(int64) :: cost

      ! 1 to 4 directly costs 100; via 2 and 3 it costs 30.
      call graph%build(4_int32, &
                       [1_id_k, 1_id_k, 2_id_k, 3_id_k], &
                       [4_id_k, 2_id_k, 3_id_k, 4_id_k], &
                       [100_int32, 10_int32, 10_int32, 10_int32], err)
      call graph%shortest_path(1_id_k, 4_id_k, route, route_len, cost, err)
      call check(error,.not. err%has_error(), "shortest_path reported an error")
      if (allocated(error)) return

      call check(error, cost == 30_int64, "took the expensive direct edge")
      if (allocated(error)) return
      call check(error, route_len == 4_int32, "route length")
   end subroutine test_prefers_cheaper

   subroutine test_unreachable(error)
      !! An island reports no route rather than failing.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err
      integer(id_k) :: route(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: route_len
      integer(int64) :: cost

      ! Node 3 is joined to nothing.
      call graph%build(3_int32, [1_id_k], [2_id_k], [10_int32], err)
      call graph%shortest_path(1_id_k, 3_id_k, route, route_len, cost, err)

      call check(error,.not. err%has_error(), "unreachable should not be an error")
      if (allocated(error)) return
      call check(error, route_len == 0_int32, "route length should be zero")
      if (allocated(error)) return
      call check(error, cost == -1_int64, "cost should be -1")
   end subroutine test_unreachable

   subroutine test_bad_endpoint(error)
      !! A node outside the graph is a validation error.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: graph
      type(error_t) :: err, path_err
      integer(id_k) :: route(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: route_len
      integer(int64) :: cost

      call build_line(graph, err)
      call graph%shortest_path(1_id_k, 99_id_k, route, route_len, cost, path_err)

      call check(error, path_err%has_error(), "an out-of-range target should be an error")
      if (allocated(error)) return
      call check(error, route_len == 0_int32, "route length should be zero")
   end subroutine test_bad_endpoint

   subroutine test_edge_order(error)
      !! The same airport read in a different order routes identically.
      !!
      !! This is the property that lets a loader be rewritten without retiring
      !! every golden expectation.
      type(error_type), allocatable, intent(out) :: error

      type(taxi_graph_t) :: forwards, backwards
      type(error_t) :: err
      integer(id_k) :: route_a(AIRCRAFT_ROUTE_ROWS), route_b(AIRCRAFT_ROUTE_ROWS)
      integer(int32) :: len_a, len_b
      integer(int64) :: cost_a, cost_b

      call forwards%build(4_int32, [1_id_k, 2_id_k, 3_id_k], [2_id_k, 3_id_k, 4_id_k], &
                          [10_int32, 20_int32, 30_int32], err)
      call backwards%build(4_int32, [3_id_k, 2_id_k, 1_id_k], [4_id_k, 3_id_k, 2_id_k], &
                           [30_int32, 20_int32, 10_int32], err)

      call forwards%shortest_path(1_id_k, 4_id_k, route_a, len_a, cost_a, err)
      call backwards%shortest_path(1_id_k, 4_id_k, route_b, len_b, cost_b, err)

      call check(error, len_a == len_b, "route lengths differ")
      if (allocated(error)) return
      call check(error, cost_a == cost_b, "route costs differ")
      if (allocated(error)) return
      call check(error, all(route_a(1:len_a) == route_b(1:len_b)), "routes differ")
   end subroutine test_edge_order

end module test_core_graph
