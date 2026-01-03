#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day16.h"

std::vector<std::string> rows{
    "Before: [3, 2, 1, 1]",
    "9 2 1 2",
    "After:  [3, 2, 2, 1]",
    ""
};

TEST_CASE("part1") {
    REQUIRE(day16::part1(rows) == 1);
}

TEST_CASE("part2") {
    REQUIRE(day16::part2(rows) == -1);
}
