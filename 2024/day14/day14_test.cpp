#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day14.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day14::part1(rows) == -1);
}
