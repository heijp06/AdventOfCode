#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day04.h"

std::vector<std::string> rows{
    "..@@.@@@@.",
    "@@@.@.@.@@",
    "@@@@@.@.@@",
    "@.@@@@..@.",
    "@@.@@@@.@@",
    ".@@@@@@@.@",
    ".@.@.@.@@@",
    "@.@@@.@@@@",
    ".@@@@@@@@.",
    "@.@.@@@.@.",
};

TEST_CASE("part1") {
    REQUIRE(day04::part1(rows) == 13);
}
