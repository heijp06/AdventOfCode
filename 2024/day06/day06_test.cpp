#include "../../lib/catch.hpp"
#include "day06.h"

#include <string>
#include <vector>

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
