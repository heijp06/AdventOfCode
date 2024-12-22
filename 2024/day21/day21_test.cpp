#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day21.h"

std::vector<std::string> rows{
    "029A",
    "980A",
    "179A",
    "456A",
    "379A"
};

TEST_CASE("part1") {
    REQUIRE(day21::part1(rows) == 126384);
}
