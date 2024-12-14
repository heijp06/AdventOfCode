#include "../../lib/catch.hpp"
#include "day02.h"

#include <string>
#include <vector>

TEST_CASE("part1") {
    const std::vector<std::string>& ids = {
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

TEST_CASE("part2") {
    const std::vector<std::string>& ids = {
        "abcde",
        "fghij",
        "klmno",
        "pqrst",
        "fguij",
        "axcye",
        "wvxyz",
    };

    REQUIRE(day02::part2(ids) == "fgij");
}
