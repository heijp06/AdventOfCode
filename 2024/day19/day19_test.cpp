#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day19.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day19::part1(rows) == -1);
}
