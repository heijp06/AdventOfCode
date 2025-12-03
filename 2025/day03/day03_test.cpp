#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day03.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day03::part1(rows) == -1);
}
