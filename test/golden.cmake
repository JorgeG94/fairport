# Run one scenario and compare its whole output against a committed file.
#
# `./fairport scenario.txt --seed 42 | diff - expected.txt`, as the design
# document puts it, with the diff printed on failure so the change is readable
# rather than just reported.

execute_process(
  COMMAND "${FAIRPORT}" "${SCENARIO}" --seed 42
  OUTPUT_VARIABLE actual
  ERROR_VARIABLE errors
  RESULT_VARIABLE status)

if(NOT status EQUAL 0)
  message(FATAL_ERROR "fairport failed: ${errors}")
endif()

file(READ "${GOLDEN}" expected)

if(NOT actual STREQUAL expected)
  # SCRATCH is handed in from the build tree: in -P script mode
  # CMAKE_CURRENT_BINARY_DIR is just the working directory, which is the source
  # tree, and a failing test must not litter it.
  set(scratch "${SCRATCH}")
  file(WRITE "${scratch}" "${actual}")
  execute_process(
    COMMAND diff -u "${GOLDEN}" "${scratch}"
    OUTPUT_VARIABLE delta
    ERROR_QUIET)
  message(
    FATAL_ERROR
      "output no longer matches ${GOLDEN}\n"
      "If the change is intended, regenerate with:\n"
      "  ./build/fairport scenarios/clear_day.txt --seed 42 > test/golden/clear_day_seed42.txt\n\n"
      "${delta}")
endif()

message(STATUS "golden output matches")
