# Basic CMake project
cmake_minimum_required(VERSION 3.10)

# Get the day name from the current directory
get_filename_component(day ${CMAKE_CURRENT_SOURCE_DIR} NAME)
cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH parent)
get_filename_component(year ${parent} NAME)

# Name the project after the day
project(${day} CXX)

# Implementation could be only a header
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${day}.cpp)
    set(day_cpp ${day}.cpp)
else()
    set(day_cpp "")
endif()

# Build executable from sources and headers
add_executable(${year}_${day}_test ${day}_test.cpp ${day_cpp} ${day}.h ../../lib/tests-main.cpp ../../lib/advent.cpp)
add_executable(${year}_${day} ${day}_main.cpp ${day_cpp} ${day}.h ../../lib/advent.cpp)

set_target_properties(${year}_${day} PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED OFF
    CXX_EXTENSIONS OFF
)

set_target_properties(${year}_${day}_test PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED OFF
    CXX_EXTENSIONS OFF
)

set(CMAKE_BUILD_TYPE Debug)

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "(GNU|Clang)")
    set_target_properties(${year}_${day} PROPERTIES
        COMPILE_FLAGS "-Wall -Wextra -Wpedantic -Werror"
    )
endif()

# Tell MSVC not to warn us about unchecked iterators in debug builds
if(${MSVC})
    set_target_properties(${year}_${day} PROPERTIES
        COMPILE_DEFINITIONS_DEBUG _SCL_SECURE_NO_WARNINGS
        COMPILE_FLAGS "/WX /w44244 /w44267 /w44996")
endif()

# Run the tests on every build
add_custom_target(test_${year}_${day} ALL DEPENDS ${year}_${day}_test COMMAND ${year}_${day}_test)

# Do not run the main executables in GitHub CI because the data directory is not filled.
if (NOT DEFINED ENV{CI})
    add_custom_target(main_${year}_${day} ALL DEPENDS ${year}_${day} COMMAND ${year}_${day})
endif()
