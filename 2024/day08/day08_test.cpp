#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day08.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day08::part1(rows) == -1);
}
