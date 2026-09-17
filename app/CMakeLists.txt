# The app layer is a library with a thin program on top, so that the loader, the
# scenario parser and the text formatter can be tested directly. They are the
# least pleasant code in the project and the most likely to be wrong in a way
# only a malformed input finds.
set(fairport_app_sources app_text.f90 app_airport.f90 app_schedule.f90
                         app_render.f90 app_scenario.f90)

# The interactive board is the only source needing pic's terminal layer, so it
# is compiled only when that layer is built. Everything else, the whole test
# suite included, is unaffected by the option.
if(FAIRPORT_ENABLE_TUI)
  list(APPEND fairport_app_sources app_tui.f90)
endif()

add_library(fairport_app STATIC ${fairport_app_sources})
add_library(fairport::app ALIAS fairport_app)

set_target_properties(fairport_app PROPERTIES Fortran_MODULE_DIRECTORY
                                              "${fairport_module_dir}")
target_include_directories(fairport_app PUBLIC "${fairport_module_dir}"
                                               "${pic_module_dir}")
target_compile_options(fairport_app PRIVATE ${fairport_flags}
                                            ${fairport_warnings})
if(FAIRPORT_ENABLE_TUI)
  target_compile_definitions(fairport_app PUBLIC FAIRPORT_TUI)
endif()
target_link_libraries(fairport_app PUBLIC fairport::core toml-f::toml-f-lib)

add_executable(fairport main.F90)

set_target_properties(fairport PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                          "${CMAKE_BINARY_DIR}")
target_compile_options(fairport PRIVATE ${fairport_flags} ${fairport_warnings})
target_link_libraries(fairport PRIVATE fairport::app)
