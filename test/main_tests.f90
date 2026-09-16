! SPDX-License-Identifier: MIT
! Copyright (c) 2026 Jorge Luis Galvez Vallejo
program fairport_tester
   !! Test runner. Pass a suite name to run one suite, and a test name after it
   !! to run one test.
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
                        select_suite, run_selected, get_argument
   use pic_types, only: int32
   use test_core_graph, only: collect_core_graph_tests
   use test_core_scheduler, only: collect_core_scheduler_tests
   use test_core_bus, only: collect_core_bus_tests
   use test_core_aircraft, only: collect_core_aircraft_tests
   use test_core_command, only: collect_core_command_tests
   use test_core_time, only: collect_core_time_tests
   use test_sys_arrival, only: collect_sys_arrival_tests
   use test_sys_departure, only: collect_sys_departure_tests
   use test_core_determinism, only: collect_core_determinism_tests
   use test_app_text, only: collect_app_text_tests
   use test_app_loader, only: collect_app_loader_tests
   use test_app_schedule, only: collect_app_schedule_tests
   implicit none

   integer(int32) :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: style = '("#", *(1x, a))'

   stat = 0_int32

   ! Allocated first and assigned second: some compilers object to allocating
   ! on the fly from an array constructor of derived types.
   allocate (testsuites(12))
   testsuites = [ &
                new_testsuite("core_graph", collect_core_graph_tests), &
                new_testsuite("core_scheduler", collect_core_scheduler_tests), &
                new_testsuite("core_bus", collect_core_bus_tests), &
                new_testsuite("core_aircraft", collect_core_aircraft_tests), &
                new_testsuite("core_command", collect_core_command_tests), &
                new_testsuite("core_time", collect_core_time_tests), &
                new_testsuite("sys_arrival", collect_sys_arrival_tests), &
                new_testsuite("sys_departure", collect_sys_departure_tests), &
                new_testsuite("core_determinism", collect_core_determinism_tests), &
                new_testsuite("app_text", collect_app_text_tests), &
                new_testsuite("app_loader", collect_app_loader_tests), &
                new_testsuite("app_schedule", collect_app_schedule_tests) &
                ]

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, style) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) error stop 1
         else
            write (error_unit, style) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, style) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, style) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, style) "Testing all:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
      error stop 1
   end if

end program fairport_tester
