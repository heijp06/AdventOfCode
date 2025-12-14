#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

const std::vector<std::string> rows{
    "7,1",
    "11,1",
    "11,7",
    "9,7",
    "9,5",
    "2,5",
    "2,3",
    "7,3",
};

TEST_CASE("part1") {
    REQUIRE(day09::part1(rows) == 50);
}

TEST_CASE("part2") {
    REQUIRE(day09::part2(rows) == 24);
}

