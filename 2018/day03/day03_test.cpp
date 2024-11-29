#include "../lib/catch.hpp"
#include "day03.h"

TEST_CASE("part1") {
	std::vector<std::string> rows = {
		"#1 @ 1,3: 4x4",
		"#2 @ 3,1: 4x4",
		"#3 @ 5,5: 2x2"
	};

	const auto expected = 4;
	auto actual = day03::part1(rows);

    // REQUIRE(actual == expected);
    REQUIRE(actual == -1);
}
