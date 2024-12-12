#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day11.h"

std::vector<std::string> rows{"125 17"};

TEST_CASE("part1") {
    REQUIRE(day11::part1(rows) == 55312);
}
