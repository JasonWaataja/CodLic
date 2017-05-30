set (SOURCES main.cc options.cc licensor.cc)
add_executable (codlic ${SOURCES})
set_property(TARGET codlic PROPERTY CXX_STANDARD 11)
install (TARGETS codlic DESTINATION bin)
