#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day11.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day11::part1(rows) == -1);
}
