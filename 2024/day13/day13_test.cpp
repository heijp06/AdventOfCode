#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day13.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day13::part1(rows) == -1);
}
