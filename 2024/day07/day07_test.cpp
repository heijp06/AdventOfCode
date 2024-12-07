#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day07.h"

std::vector<std::string> rows{
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
};

TEST_CASE("part1") {
    REQUIRE(day07::part1(rows) == 3749);
}
