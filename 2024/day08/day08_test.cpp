#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day08.h"

std::vector<std::string> rows{
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
};

TEST_CASE("part1") {
    REQUIRE(day08::part1(rows) == 14);
}

TEST_CASE("part2") {
    REQUIRE(day08::part2(rows) == 34);
}
