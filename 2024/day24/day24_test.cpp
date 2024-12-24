#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day24.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day24::part1(rows) == -1);
}
