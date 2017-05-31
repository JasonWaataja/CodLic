set (SOURCES main.cc options.cc licensor.cc filetypes.cc logging.cc
	comment_type.cc license.cc)

configure_file (config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)
include_directories (${CMAKE_CURRENT_BINARY_DIR})

add_executable (codlic ${SOURCES})
set_property(TARGET codlic PROPERTY CXX_STANDARD 11)
install (TARGETS codlic DESTINATION bin)
