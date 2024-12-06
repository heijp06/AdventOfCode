#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day06.h"

std::vector<std::string> rows{
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#...",
};

TEST_CASE("part1") {
    REQUIRE(day06::part1(rows) == 41);
}

TEST_CASE("part2") {
    REQUIRE(day06::part2(rows) == 6);
}
