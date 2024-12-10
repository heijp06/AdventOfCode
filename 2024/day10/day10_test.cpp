#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day10.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day10::part1(rows) == -1);
}
