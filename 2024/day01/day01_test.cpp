#include "../../lib/catch.hpp"
#include "day01.h"

#include <vector>
#include <string>

TEST_CASE("part1") {
    std::vector<std::string> rows = {
		"3   4",
		"4   3",
		"2   5",
		"1   3",
		"3   9",
		"3   3"
    };

    REQUIRE(day01::part1(rows) == 11);
}
