#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day07.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day07::part1(rows) == -1);
}
