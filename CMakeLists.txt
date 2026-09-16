cmake_minimum_required(VERSION 3.28)

project(
  "fairport"
  LANGUAGES Fortran
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
    "1256ca40"
    CACHE STRING "pic commit to pin; v0.8.0")

include(FetchContent)

# pic's own suite is not fairport's to run, and turning it off keeps pic's
# test-drive fetch out of a fairport build.
set(PIC_ENABLE_TESTING
    OFF
    CACHE BOOL "" FORCE)

FetchContent_Declare(
  pic
  GIT_REPOSITORY "${FAIRPORT_PIC_REPOSITORY}"
  GIT_TAG "${FAIRPORT_PIC_TAG}")
FetchContent_MakeAvailable(pic)

# pic writes its .mod files to ${CMAKE_BINARY_DIR}/modules, and fairport joins
# it there so that one include directory covers both.
set(fairport_module_dir "${CMAKE_BINARY_DIR}/modules")

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
else()
  set(fairport_flags)
  set(fairport_warnings)
endif()

add_library(fairport_core STATIC)
add_library(fairport::core ALIAS fairport_core)

add_subdirectory(src/core)

set_target_properties(fairport_core PROPERTIES Fortran_MODULE_DIRECTORY
                                               "${fairport_module_dir}")
target_include_directories(fairport_core PUBLIC "${fairport_module_dir}")
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
message(STATUS "============================================")
message(STATUS "")
