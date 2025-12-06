# Basic CMake project
cmake_minimum_required(VERSION 3.5.1)

# Name the project advent
project(advent CXX)

# Build executable from sources and headers
add_executable(advent_test advent_test.cpp)

add_library(adventlib advent.cpp advent.h)
add_library(testsmain tests-main.cpp)

set_target_properties(advent_test PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED OFF
    CXX_EXTENSIONS OFF
)

set_target_properties(adventlib PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED OFF
    CXX_EXTENSIONS OFF
)

set_target_properties(testsmain PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED OFF
    CXX_EXTENSIONS OFF
)

target_link_libraries(advent_test adventlib testsmain)

set(CMAKE_BUILD_TYPE Debug)

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "(GNU|Clang)")
    set_target_properties(advent_test PROPERTIES
        COMPILE_FLAGS "-Wall -Wextra -Wpedantic -Werror"
    )
endif()

# Tell MSVC not to warn us about unchecked iterators in debug builds
if(${MSVC})
    set_target_properties(advent_test PROPERTIES
        COMPILE_DEFINITIONS_DEBUG _SCL_SECURE_NO_WARNINGS
        COMPILE_FLAGS "/WX /w44244 /w44267")
endif()

# Run the tests on every build
add_custom_target(test_advent ALL DEPENDS advent_test COMMAND advent_test)
