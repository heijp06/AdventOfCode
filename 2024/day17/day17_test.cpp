#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day17.h"

std::vector<std::string> rows{
    "Register A: 46187030",
    "Register B: 0",
    "Register C: 0",
    "",
    "Program: 2,4,1,5,7,5,0,3,4,0,1,6,5,5,3,0"
};

TEST_CASE("part1") {
    REQUIRE(day17::part1(rows) == "4,6,3,5,6,3,5,2,1,0");
}
