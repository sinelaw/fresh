# CMake syntax highlighting test
cmake_minimum_required(VERSION 3.20)
project(HelloWorld VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_TESTS "Build test suite" ON)

add_executable(hello
    src/main.cpp
    src/utils.cpp
)

target_include_directories(hello PRIVATE
    ${CMAKE_SOURCE_DIR}/include
)

if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS hello DESTINATION bin)
