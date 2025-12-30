#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day08.h"

std::vector<std::string> rows{
    "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
};

TEST_CASE("part1") {
    REQUIRE(day08::part1(rows) == 138);
}

TEST_CASE("part2") {
    REQUIRE(day08::part2(rows) == 66);
}
