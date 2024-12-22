#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day22.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day22::part1(rows) == -1);
}
