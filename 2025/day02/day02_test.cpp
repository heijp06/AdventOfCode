#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day02.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day02::part1(rows) == -1);
}
