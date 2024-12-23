#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day23.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day23::part1(rows) == -1);
}
