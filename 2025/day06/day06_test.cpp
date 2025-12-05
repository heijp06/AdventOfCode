#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day06.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day06::part1(rows) == -1);
}
