# The app layer is a library with a thin program on top, so that the loader, the
# scenario parser and the text formatter can be tested directly. They are the
# least pleasant code in the project and the most likely to be wrong in a way
# only a malformed input finds.
add_library(fairport_app STATIC app_text.f90 app_airport.f90 app_schedule.f90
                                app_render.f90 app_scenario.f90)
add_library(fairport::app ALIAS fairport_app)

set_target_properties(fairport_app PROPERTIES Fortran_MODULE_DIRECTORY
                                              "${fairport_module_dir}")
target_include_directories(fairport_app PUBLIC "${fairport_module_dir}")
target_compile_options(fairport_app PRIVATE ${fairport_flags}
                                            ${fairport_warnings})
target_link_libraries(fairport_app PUBLIC fairport::core toml-f::toml-f-lib)

add_executable(fairport main.f90)

set_target_properties(fairport PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                          "${CMAKE_BINARY_DIR}")
target_compile_options(fairport PRIVATE ${fairport_flags} ${fairport_warnings})
target_link_libraries(fairport PRIVATE fairport::app)
