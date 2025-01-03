#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day03.h"

TEST_CASE("part1") {
    std::vector<std::string> rows = {
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    };

    REQUIRE(day03::part1(rows) == 161);
}

TEST_CASE("part2") {
    std::vector<std::string> rows = {
        "xmul(2,4)& mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    };

    REQUIRE(day03::part2(rows) == 48);
}
