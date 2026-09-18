# Compiler flags, one block per front end.
#
# Two sets, and the difference between them is which side of the dependency
# boundary they land on.
#
# `FAIRPORT_FLAGS_GLOBAL` goes into `CMAKE_Fortran_FLAGS`, so pic, toml-f and
# test-drive are built with it too. That is only ever for flags the *language*
# needs -- a compiler that does not implement something by default -- because
# anything else here is fairport imposing its taste on somebody else's source.
#
# `FAIRPORT_FLAGS_WARNINGS` is applied per target, to fairport's own targets
# alone. It carries `-Werror`, and holding a fetched dependency to fairport's
# warning settings would break the build on a dependency bump that fairport had
# nothing to do with.
#
# `FAIRPORT_FLAGS_STANDARD` is per target as well: language-level pedantry like
# `-std=f2018` belongs to fairport's sources, not to pic's.
#
# Included before `FetchContent_MakeAvailable`, because a global flag set after
# a dependency is configured does not reach it.

# --------------------------------------------------------------- build type --
#
# Release when nothing is asked for. Not a tuning preference: RelWithDebInfo
# passes `-g`, and LFortran's debug-info path shells out to `llvm-dwarfdump`,
# which the conda environment does not ship. The failure surfaces inside pic's
# `app` target as "Error in creating the files used to generate the debug
# information", which names neither the cause nor fairport, and costs an hour
# the first time.
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Build type" FORCE)
  message(STATUS "No build type given; using '${CMAKE_BUILD_TYPE}'.")
endif()

# ------------------------------------------------------------- per compiler --
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(FAIRPORT_FLAGS_GLOBAL "")
  set(FAIRPORT_FLAGS_STANDARD "-std=f2018" "-ffree-line-length-none"
                              "-fbacktrace")
  set(FAIRPORT_FLAGS_WARNINGS
      "-Wall"
      "-Wextra"
      "-Werror"
      "-fimplicit-none"
      # `self` is genuinely unused in a constant accessor like tick_order.
      "-Wno-unused-dummy-argument"
      # gfortran cannot see that `x = f()` allocates an allocatable array of a
      # derived type, and reports every tokenize result as maybe-uninitialized
      # once the optimiser is on. A false positive, and the only one, so the
      # rest of -Werror stays worth having.
      "-Wno-maybe-uninitialized")
  # No -ffpe-trap. fairport's core is integer-only by design, so there is no
  # floating-point exception for it to catch, and pic's own tokenizer and
  # NaN-canonicalisation tests trap under it.

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
  set(FAIRPORT_FLAGS_GLOBAL "")
  set(FAIRPORT_FLAGS_STANDARD "-stand" "f18" "-traceback")
  set(FAIRPORT_FLAGS_WARNINGS "-warn" "all")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LFortran")
  # Automatic reallocation on assignment is standard since Fortran 2003 and
  # LFortran does not do it unless asked; without it `x = [ ... ]` fails at run
  # time with "array is not allocated" rather than at compile time. That is the
  # compiler implementing the language, so it is global: set per target it fixes
  # fairport and leaves test-drive's and toml-f's own test programs failing the
  # same way.
  set(FAIRPORT_FLAGS_GLOBAL "--realloc-lhs-arrays")
  set(FAIRPORT_FLAGS_STANDARD "")
  # No warning set. LFortran has no -Wall equivalent to hold to yet.
  set(FAIRPORT_FLAGS_WARNINGS "")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang|Flang")
  # Builds, but has never been run to a digest. Here so that a build with it
  # fails on the source rather than on the flags.
  set(FAIRPORT_FLAGS_GLOBAL "")
  set(FAIRPORT_FLAGS_STANDARD "")
  set(FAIRPORT_FLAGS_WARNINGS "")

else()
  message(WARNING "Unknown Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}; "
                  "building with no flags of fairport's own.")
  set(FAIRPORT_FLAGS_GLOBAL "")
  set(FAIRPORT_FLAGS_STANDARD "")
  set(FAIRPORT_FLAGS_WARNINGS "")
endif()

# ------------------------------------------------------------------- apply --
#
# `include()` runs in the caller's scope, so there is no PARENT_SCOPE here and
# no chance of the trap that comes with it: `set(... PARENT_SCOPE)` leaves the
# local alone, so two in a row means the second silently discards the first.
foreach(flag IN LISTS FAIRPORT_FLAGS_GLOBAL)
  string(APPEND CMAKE_Fortran_FLAGS " ${flag}")
endforeach()

# What the targets read. `fairport_flags` and `fairport_warnings` are the names
# `app/`, `test/` and `src/core/` already use.
set(fairport_flags ${FAIRPORT_FLAGS_STANDARD})
set(fairport_warnings ${FAIRPORT_FLAGS_WARNINGS})
