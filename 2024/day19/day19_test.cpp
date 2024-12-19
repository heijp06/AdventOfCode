#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day19.h"

std::vector<std::string> rows{
    "r, wr, b, g, bwu, rb, gb, br",
    "",
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb"
};

TEST_CASE("part1") {
    REQUIRE(day19::part1(rows) == 6);
}

// 96771039: Too low.
TEST_CASE("part2") {
    REQUIRE(day19::part2(rows) == 16);
}
