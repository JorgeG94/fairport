# Save a run, replay the save, and require the two to be the same day.
#
# The claim being tested is the one `log_write_to` makes in its own docstring:
# the save file is the input format. It went untested for a milestone, and it
# was false -- the writer emitted raw milliseconds and the parser only accepted
# HH:MM. Nothing caught it because nothing had ever saved a session.
#
# This matters most for a session played on the interactive board, which is
# otherwise unrepeatable: it happened, and there is nothing to hand anybody. The
# scenarios stand in for that here because a ctest has no terminal, but the path
# through save_session and the parser is the same one the board uses.
#
# Both halves are checked. The digest is the simulation agreeing; the console
# output is the report agreeing, which catches a horizon written back one tick
# short -- a difference the digest would show as nothing at all if the missing
# tick held no events.

function(fairport_run out_var)
  execute_process(
    COMMAND ${ARGN}
    OUTPUT_VARIABLE output
    ERROR_VARIABLE errors
    RESULT_VARIABLE status
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT status EQUAL 0)
    message(FATAL_ERROR "fairport failed: ${errors}")
  endif()
  set(${out_var}
      "${output}"
      PARENT_SCOPE)
endfunction()

fairport_run(
  original_digest
  "${FAIRPORT}"
  "${SCENARIO}"
  --seed
  42
  --save
  "${SAVE}"
  --hash)
fairport_run(replay_digest "${FAIRPORT}" "${SAVE}" --hash)

if(NOT original_digest STREQUAL replay_digest)
  message(
    FATAL_ERROR
      "the save does not replay: ${SCENARIO} gave ${original_digest}, "
      "${SAVE} gave ${replay_digest}")
endif()

fairport_run(original_report "${FAIRPORT}" "${SCENARIO}" --seed 42)
fairport_run(replay_report "${FAIRPORT}" "${SAVE}")

if(NOT original_report STREQUAL replay_report)
  message(FATAL_ERROR "the save replays to a different report; see ${SAVE}")
endif()

message(STATUS "round trip -> ${original_digest}")
