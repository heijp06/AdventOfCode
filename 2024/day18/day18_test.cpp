#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day18.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day18::part1(rows) == -1);
}
