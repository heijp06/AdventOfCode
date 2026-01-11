#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day17.h"

std::vector<std::string> rows{
    "x=495, y=2..7",
    "y=7, x=495..501",
    "x=501, y=3..7",
    "x=498, y=2..4",
    "x=506, y=1..2",
    "x=498, y=10..13",
    "x=504, y=10..13",
    "y=13, x=498..504"
};

TEST_CASE("part1") {
    REQUIRE(day17::part1(rows) == 57);
}

TEST_CASE("part2") {
    REQUIRE(day17::part2(rows) == -1);
}

TEST_CASE("extra") {
    advent::grid grid{{
        ".....+.....",
        "...........",
        ".#.......#.",
        ".#..###..#.",
        ".#..#.#..#.",
        ".#..###..#.",
        ".#.......#.",
        ".#########."
    }};

    REQUIRE(day17::solve(grid) == 49);
}
