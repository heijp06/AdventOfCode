#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day09::part1(rows) == -1);
}
