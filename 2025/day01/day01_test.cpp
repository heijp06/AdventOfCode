#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day01.h"

std::vector<std::string> rows{
"L68",
"L30",
"R48",
"L5",
"R60",
"L55",
"L1",
"L99",
"R14",
"L82",
};

TEST_CASE("part1") {
    REQUIRE(day01::part1(rows) == 3);
}
