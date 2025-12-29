#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day06.h"

std::vector<std::string> rows{"1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"};

TEST_CASE("part1") {
    REQUIRE(day06::part1(rows) == 17);
}

TEST_CASE("part2") {
    REQUIRE(day06::part2(rows) == -1);
}
