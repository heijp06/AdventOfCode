#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day12.h"

std::vector<std::string> rows{
};

TEST_CASE("part1") {
    REQUIRE(day12::part1(rows) == 0);
}

TEST_CASE("part2") {
    REQUIRE(day12::part2(rows) == -1);
}
