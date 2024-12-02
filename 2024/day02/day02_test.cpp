#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day02.h"

std::vector<std::string> rows = {
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
};

TEST_CASE("part1") {
    REQUIRE(day02::part1(rows) == 2);
}
