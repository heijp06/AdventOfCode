#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day01.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day01::part1(rows) == -1);
}
