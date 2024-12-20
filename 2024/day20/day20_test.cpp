#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day20.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day20::part1(rows) == -1);
}
