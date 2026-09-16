# Run one scenario three times and check the digests behave.
#
# Two runs with the same seed must agree; a run with a different seed must not.
# The second half matters as much as the first: a simulation that ignored its
# random streams entirely would pass the repeatability check perfectly.
#
# The cross-compiler version of this -- the fan-in job that compares digests
# from gfortran, ifx, flang and LFortran -- belongs in CI, where four different
# front ends actually exist.

function(run_hash seed out_var)
  execute_process(
    COMMAND "${FAIRPORT}" "${SCENARIO}" --seed "${seed}" --hash
    OUTPUT_VARIABLE output
    ERROR_VARIABLE errors
    RESULT_VARIABLE status
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT status EQUAL 0)
    message(FATAL_ERROR "fairport failed with seed ${seed}: ${errors}")
  endif()
  string(STRIP "${output}" output)
  if(NOT output MATCHES "^[0-9a-f]+$")
    message(
      FATAL_ERROR "seed ${seed} did not print a bare digest, got: '${output}'")
  endif()
  set(${out_var}
      "${output}"
      PARENT_SCOPE)
endfunction()

run_hash(42 first)
run_hash(42 second)
run_hash(4242 other)

if(NOT first STREQUAL second)
  message(
    FATAL_ERROR "DETERMINISM BROKEN: seed 42 gave ${first} then ${second}")
endif()

if(first STREQUAL other)
  message(
    FATAL_ERROR "the seed made no difference: 42 and 4242 both gave ${first}")
endif()

message(STATUS "seed 42   -> ${first}")
message(STATUS "seed 4242 -> ${other}")
