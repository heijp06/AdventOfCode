#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day15.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day15::part1(rows) == -1);
}
