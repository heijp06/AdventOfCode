#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day03.h"

std::vector<std::string> rows{
    "987654321111111",
    "811111111111119",
    "234234234234278",
    "818181911112111",
};

TEST_CASE("part1") {
    REQUIRE(day03::part1(rows) == 357);
}

TEST_CASE("part2") {
    REQUIRE(day03::part2(rows) == 3121910778619L);
}
