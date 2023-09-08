cmake_minimum_required(VERSION 3.27)
project(test)
set(CMAKE_INSTALL_MESSAGE LAZY)  # Don't tell us about up-to-date files.
set(CMAKE_CXX_FLAGS "-ggdb")
set(CMAKE_C_FLAGS "-ggdb")
set(CMAKE_CXX_FLAGS_DEBUG "-ggdb")
set(CMAKE_C_FLAGS_DEBUG "-ggdb")
find_package(Catch2 3 REQUIRED)


add_executable(test test.cpp)
target_link_libraries(test PRIVATE Catch2::Catch2WithMain ${CMAKE_SOURCE_DIR}/../emulator_lib/lib_emulator.a)

