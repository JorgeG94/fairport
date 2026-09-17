cmake_minimum_required(VERSION 3.28)

# The interactive terminal is opt-in.
#
# It is the only part of fairport that compiles C -- pic's terminal layer is a
# single C file holding every operating-system conditional in either project --
# so a build that does not want it does not build it, and the test suite, batch
# mode and the determinism digest are all unaffected either way.
#
# It does not save you needing a C compiler: pic declares `LANGUAGES Fortran C`
# unconditionally, so consuming pic at all requires one. What this option
# controls is whether pic's `term/` directory is compiled and whether
# `app_tui.f90` exists in this build.
option(FAIRPORT_ENABLE_TUI
       "Build the interactive terminal; requires a C compiler" OFF)

if(FAIRPORT_ENABLE_TUI)
  set(fairport_languages Fortran C)
else()
  set(fairport_languages Fortran)
endif()

project(
  "fairport"
  LANGUAGES ${fairport_languages}
  VERSION 0.1.0
  DESCRIPTION "Deterministic airport operations simulator")
set(project_name fairport)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/modules")

option(FAIRPORT_ENABLE_TESTING "Enable testing for fairport" ON)

# pic is fairport's standard library, fetched and pinned.
#
# Pinned to a commit rather than a tag on purpose. fairport's one invariant is
# that a run is reproducible from (seed, command log); a tag can be moved, and a
# moved tag would silently change the event-log digest under a build that looks
# unchanged. The SHA below is v0.8.0.
#
# To build against a local pic checkout instead -- which is what you want when
# changing both at once -- configure with:
#
# -DFETCHCONTENT_SOURCE_DIR_PIC=/path/to/pic
#
# That is FetchContent's own override; it skips the download and uses the tree
# as-is, so there is no second code path here to drift.
set(FAIRPORT_PIC_REPOSITORY
    "https://github.com/JorgeG94/pic.git"
    CACHE STRING "Git repository to fetch pic from")
set(FAIRPORT_PIC_TAG
    "82785ccf"
    CACHE STRING "pic commit to pin; v0.8.2")

include(FetchContent)

# pic's own suite is not fairport's to run, and turning it off keeps pic's
# test-drive fetch out of a fairport build.
set(PIC_ENABLE_TESTING
    OFF
    CACHE BOOL "" FORCE)

# pic's terminal layer follows fairport's own switch. It is pic's only C source
# and its only place with operating-system conditionals, so a build that does
# not want the interactive terminal does not pay for either.
set(PIC_ENABLE_TERM
    ${FAIRPORT_ENABLE_TUI}
    CACHE BOOL "" FORCE)

FetchContent_Declare(
  pic
  GIT_REPOSITORY "${FAIRPORT_PIC_REPOSITORY}"
  GIT_TAG "${FAIRPORT_PIC_TAG}")
FetchContent_MakeAvailable(pic)

# toml-f reads the schedule. Pinned to a release tag rather than a branch, for
# the same reason pic is pinned to a SHA: a dependency that moves underneath a
# build can change what a scenario does without anything in this repository
# changing.
#
# Only `app/` may use it. A TOML parser allocates, raises its own errors and
# reads files -- three things `core/` does not do -- and the layering check
# enforces that.
set(FAIRPORT_TOMLF_TAG
    "v0.5.2"
    CACHE STRING "toml-f release to pin")

FetchContent_Declare(
  toml-f
  GIT_REPOSITORY "https://github.com/toml-f/toml-f.git"
  GIT_TAG "${FAIRPORT_TOMLF_TAG}")
FetchContent_MakeAvailable(toml-f)

# Where the .mod files live.
#
# fairport keeps its own in the top-level build. pic keeps its own in *its*
# build directory, which FetchContent hands back as `pic_BINARY_DIR`.
#
# Those used to be the same place: pic wrote to ${CMAKE_BINARY_DIR}/modules, the
# top-level build, and fairport simply joined it there. pic v0.8.2 moved to
# ${PROJECT_BINARY_DIR}/modules, which is correct -- a dependency writing into
# its parent's build tree is rude and collides the moment two of them do it --
# and it broke every consumer that had been relying on the old spelling,
# including this one.
#
# Asking FetchContent where pic ended up is the right question either way, and
# does not care which spelling pic uses next.
set(fairport_module_dir "${CMAKE_BINARY_DIR}/modules")
set(pic_module_dir "${pic_BINARY_DIR}/modules")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fairport_flags "-std=f2018" "-ffree-line-length-none" "-fbacktrace")
  set(fairport_warnings
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
  # No -ffpe-trap here. fairport's core is integer-only by design, so there is
  # no floating-point exception for it to catch, and pic's own tokenizer and
  # NaN-canonicalisation tests trap under it.
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
  set(fairport_flags "-stand" "f18" "-traceback")
  set(fairport_warnings "-warn" "all")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LFortran")
  # Automatic reallocation on assignment is standard since Fortran 2003 and
  # LFortran does not do it unless asked. Without this, `tokens = tokenize(..)`
  # and every `testsuite = [ ... ]` fail at run time with "array is not
  # allocated" rather than at compile time, so the flag is not a nicety.
  #
  # LFortran is not usable for fairport at 0.66.0 even with it. Anything that
  # builds a `sim_t` corrupts the heap -- `double free or corruption (fasttop)`
  # in every suite that runs a simulation, and a batch run reports no aircraft
  # ever parking while three of the four still depart. The bus holds
  # `class(system_t)` pointers into components of a `target` simulation, which
  # is legal and which gfortran and ifx both get right. This flag is here so the
  # next attempt starts from the real problem instead of this one.
  set(fairport_flags "--realloc-lhs-arrays")
  set(fairport_warnings)
else()
  set(fairport_flags)
  set(fairport_warnings)
endif()

add_library(fairport_core STATIC)
add_library(fairport::core ALIAS fairport_core)

add_subdirectory(src/core)

set_target_properties(fairport_core PROPERTIES Fortran_MODULE_DIRECTORY
                                               "${fairport_module_dir}")
target_include_directories(fairport_core PUBLIC "${fairport_module_dir}"
                                                "${pic_module_dir}")
target_compile_options(fairport_core PRIVATE ${fairport_flags}
                                             ${fairport_warnings})
target_link_libraries(fairport_core PUBLIC pic::pic_core)

add_subdirectory(app)

if(FAIRPORT_ENABLE_TESTING)
  enable_testing()
  set(TESTDRIVE_WITH_QP
      OFF
      CACHE BOOL "" FORCE)
  set(TESTDRIVE_WITH_XDP
      OFF
      CACHE BOOL "" FORCE)
  find_package("test-drive" REQUIRED)
  add_subdirectory(test)
endif()

message(STATUS "")
message(STATUS "========== fairport configuration ==========")
message(STATUS "  Fortran compiler   : ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "  Build type         : ${CMAKE_BUILD_TYPE}")
message(STATUS "  Testing            : ${FAIRPORT_ENABLE_TESTING}")
message(STATUS "  Interactive TUI    : ${FAIRPORT_ENABLE_TUI}")
message(STATUS "============================================")
message(STATUS "")
