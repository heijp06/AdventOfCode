#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day25.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day25::part1(rows) == -1);
}
