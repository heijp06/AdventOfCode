cmake_minimum_required(VERSION 3.5.1)
project(AoC CXX)

function(add_year year)
    file(GLOB days ${CMAKE_CURRENT_SOURCE_DIR}/${year}/day[012]*)
    foreach(day ${days})
        add_subdirectory(${day})
    endforeach()
endfunction()

add_subdirectory("lib")
add_year(2018)
add_year(2024)
