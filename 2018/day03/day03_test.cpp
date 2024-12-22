#include "../../lib/catch.hpp"
#include "day03.h"

std::vector<std::string> rows = {
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
};

TEST_CASE("part1") {
    REQUIRE(day03::part1(rows) == 4);
}

TEST_CASE("part2") {
    REQUIRE(day03::part2(rows) == 3);
}
