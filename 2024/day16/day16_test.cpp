#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day16.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day16::part1(rows) == -1);
}
