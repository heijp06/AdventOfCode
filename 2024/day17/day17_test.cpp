#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day17.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day17::part1(rows) == -1);
}
