#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day21.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day21::part1(rows) == -1);
}
