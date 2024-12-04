#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day04.h"

std::vector<std::string> rows = {
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
};

TEST_CASE("part1") {
    REQUIRE(day04::part1(rows) == 18);
}

TEST_CASE("part2") {
    REQUIRE(day04::part2(rows) == 9);
}
