#include "../lib/catch.hpp"
#include "day02.h"

#include <string>
#include <vector>

TEST_CASE("part1") {
	std::vector<std::string> ids = {
		"abcdef",
		"bababc",
		"abbcde",
		"abcccd",
		"aabcdd",
		"abcdee",
		"ababab"
	};

    REQUIRE(day02::part1(ids) == 12);
}
