#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day16.h"

std::vector<std::string> maze1 {
    "###############",
    "#.......#....E#",
    "#.#.###.#.###.#",
    "#.....#.#...#.#",
    "#.###.#####.#.#",
    "#.#.#.......#.#",
    "#.#.#####.###.#",
    "#...........#.#",
    "###.#.#####.#.#",
    "#...#.....#.#.#",
    "#.#.#.###.#.#.#",
    "#.....#...#.#.#",
    "#.###.#.#.#.#.#",
    "#S..#.....#...#",
    "###############"
};

std::vector<std::string> maze2 {
    "#################",
    "#...#...#...#..E#",
    "#.#.#.#.#.#.#.#.#",
    "#.#.#.#...#...#.#",
    "#.#.#.#.###.#.#.#",
    "#...#.#.#.....#.#",
    "#.#.#.#.#.#####.#",
    "#.#...#.#.#.....#",
    "#.#.#####.#.###.#",
    "#.#.#.......#...#",
    "#.#.###.#####.###",
    "#.#.#...#.....#.#",
    "#.#.#.#####.###.#",
    "#.#.#.........#.#",
    "#.#.#.#########.#",
    "#S#.............#",
    "#################"
};

//TEST_CASE("part1 maze 1") {
//    REQUIRE(day16::part1(maze1) == 7036);
//}
//
//TEST_CASE("part1 maze 2") {
//    REQUIRE(day16::part1(maze2) == 11048);
//}

TEST_CASE("small") {
    std::vector<std::string> maze{
        "#####",
        "#..E#",
        "#S.##",
        "#####",
    };

    REQUIRE(day16::part1(maze) == 2003);
}
