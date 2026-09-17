set(tests
    "core_graph"
    "core_reservations"
    "core_scheduler"
    "core_bus"
    "core_aircraft"
    "core_command"
    "core_time"
    "sys_arrival"
    "sys_departure"
    "sys_ops_policy"
    "sequencing"
    "core_determinism"
    "app_text"
    "app_loader"
    "app_schedule")

set(test-srcs "main_tests.f90")
foreach(t IN LISTS tests)
  list(APPEND test-srcs "test_${t}.f90")
endforeach()

add_executable(fairport-tester ${test-srcs})

set_target_properties(
  fairport-tester
  PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/test_modules"
             RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

target_include_directories(fairport-tester PRIVATE "${fairport_module_dir}"
                                                   "${pic_module_dir}")
target_compile_options(fairport-tester PRIVATE ${fairport_flags})
target_link_libraries(fairport-tester PRIVATE fairport::app
                                              test-drive::test-drive)

foreach(t IN LISTS tests)
  add_test(NAME "fairport/${t}" COMMAND $<TARGET_FILE:fairport-tester> "${t}")
endforeach()

# The end-to-end half: the binary, a real scenario, and the property the whole
# design exists to protect. Two runs of the same input must agree on the digest,
# and a different seed must not.
add_test(
  NAME "fairport/scenario_runs"
  COMMAND fairport "${CMAKE_SOURCE_DIR}/scenarios/clear_day.txt" --seed 42
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
set_tests_properties("fairport/scenario_runs" PROPERTIES PASS_REGULAR_EXPRESSION
                                                         "parked +4")

add_test(
  NAME "fairport/determinism_repeatable"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/clear_day.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/determinism.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

# The schedule's own determinism check. A bank is generated traffic, so the
# claim that a seed reproduces a timetable is worth asserting against the real
# binary and the real day1.toml, not only against a fixture.
add_test(
  NAME "fairport/determinism_full_day"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/full_day.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/determinism.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

# A session saved is a session replayable. `--save` writes the command log back
# out as an ordinary scenario, which is what turns a day played on the
# interactive board into a bug report or a regression test. costly_favour is the
# one with a command in it; clear_day is the empty-log case, where the save is
# nothing but the world and the horizon.
add_test(
  NAME "fairport/roundtrip_costly_favour"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/costly_favour.txt
    -DSAVE=${CMAKE_CURRENT_BINARY_DIR}/roundtrip_favour.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/roundtrip.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME "fairport/roundtrip_held_too_long"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/held_too_long.txt
    -DSAVE=${CMAKE_CURRENT_BINARY_DIR}/roundtrip_held.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/roundtrip.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME "fairport/roundtrip_clear_day"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/clear_day.txt
    -DSAVE=${CMAKE_CURRENT_BINARY_DIR}/roundtrip_clear.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/roundtrip.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

# The layering rules, as a test rather than as a paragraph nobody reads.
if(UNIX)
  add_test(
    NAME "fairport/layering"
    COMMAND "${CMAKE_SOURCE_DIR}/tools/check_layering.sh"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
  add_test(
    NAME "fairport/generated_sources"
    COMMAND "${CMAKE_SOURCE_DIR}/tools/autogen/check_generated.sh"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
endif()

# The golden test the design document asks for by name: a session is a file, so
# a regression is a diff. The hash test proves the event stream is stable; this
# proves the thing a person actually reads is stable too, including every column
# width in app_text. One golden per scenario. clear_day proves the no-command
# path; stand_squeeze proves the command path, and that a command log plus a
# seed is a session.
add_test(
  NAME "fairport/golden_costly_favour"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/costly_favour.txt
    -DGOLDEN=${CMAKE_SOURCE_DIR}/test/golden/costly_favour_seed42.txt
    -DSCRATCH=${CMAKE_CURRENT_BINARY_DIR}/golden_actual_favour.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/golden.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME "fairport/golden_fog_bank"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/fog_bank.txt
    -DGOLDEN=${CMAKE_SOURCE_DIR}/test/golden/fog_bank_seed42.txt
    -DSCRATCH=${CMAKE_CURRENT_BINARY_DIR}/golden_actual_fog.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/golden.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME "fairport/golden_stand_squeeze"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/stand_squeeze.txt
    -DGOLDEN=${CMAKE_SOURCE_DIR}/test/golden/stand_squeeze_seed42.txt
    -DSCRATCH=${CMAKE_CURRENT_BINARY_DIR}/golden_actual_squeeze.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/golden.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

# The new command's own golden. `hold_arrival` is the only one of the nine that
# can lose an aircraft outright, so the report line that names it -- "held at
# bingo" -- is worth pinning rather than trusting.
add_test(
  NAME "fairport/golden_held_too_long"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/held_too_long.txt
    -DGOLDEN=${CMAKE_SOURCE_DIR}/test/golden/held_too_long_seed42.txt
    -DSCRATCH=${CMAKE_CURRENT_BINARY_DIR}/golden_actual_held.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/golden.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")

add_test(
  NAME "fairport/golden_clear_day"
  COMMAND
    ${CMAKE_COMMAND} -DFAIRPORT=$<TARGET_FILE:fairport>
    -DSCENARIO=${CMAKE_SOURCE_DIR}/scenarios/clear_day.txt
    -DGOLDEN=${CMAKE_SOURCE_DIR}/test/golden/clear_day_seed42.txt
    -DSCRATCH=${CMAKE_CURRENT_BINARY_DIR}/golden_actual.txt -P
    ${CMAKE_CURRENT_SOURCE_DIR}/golden.cmake
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
