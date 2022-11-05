cmake_minimum_required(VERSION 3.5)
project(room)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

add_subdirectory(rtlsim)
