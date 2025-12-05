#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day05.h"

std::vector<std::string> rows{
    "3-5",
    "10-14",
    "16-20",
    "12-18",
    "",
    "1",
    "5",
    "8",
    "11",
    "17",
    "32",
};

TEST_CASE("part1") {
    REQUIRE(day05::part1(rows) == 3);
}
