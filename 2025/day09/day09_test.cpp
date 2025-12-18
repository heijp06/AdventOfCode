#include <cstdint>
#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

const std::vector<std::string> rows{ "7,1", "11,1", "11,7", "9,7", "9,5", "2,5", "2,3", "7,3", };

TEST_CASE("part1") {
    REQUIRE(day09::part1(rows) == 50);
}

TEST_CASE("part2") {
    REQUIRE(day09::part2(rows) == 24);
}

TEST_CASE("extra") {
    struct test_data {
        std::vector<std::string> rows;
        std::int64_t expected;
    };

    const auto& item = GENERATE(
        test_data{{"1,1", "1,2", "2,2", "2,1"}, 4},
        test_data{{"1,1", "2,1", "2,2", "3,2", "3,1", "4,1", "4,3", "4,1"}, 6}
    );

    REQUIRE(day09::part2(item.rows) == item.expected);
}

