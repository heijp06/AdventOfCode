#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day12.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day12::part1(rows) == -1);
}
