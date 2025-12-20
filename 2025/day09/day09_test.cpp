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
        test_data{{"1,1", "1,3", "3,3", "3,1"}, 9},
        test_data{{"1,1", "3,1", "3,3", "5,3", "5,1", "7,1", "7,5", "1,5"}, 15},
        test_data{{"1,3", "3,3", "3,1", "5,1", "5,3", "7,3", "7,5", "1,5"}, 21},
        test_data{{
            "1,5", "3,5", "3,3", "5,3", "5,1", "7,1", "7,3", "9,3", "9,5", "11,5",
            "11,7", "9,7", "9,9", "7,9", "7,11", "5,11", "5,9", "3,9", "3,7", "1,7"}, 49},
        test_data{{
            "5,1", "7,1", "7,3", "9,3", "9,5", "11,5",
            "11,7", "9,7", "9,9", "7,9", "7,11", "5,11"}, 35},
        test_data{{"5,1", "7,1", "7,3", "9,3", "9,5", "11,5", "11,7", "5,7"}, 25},
        test_data{{"1,1", "5,1", "5,3", "3,3", "3,5", "1,5"}, 15},
        test_data{{"1,1", "5,1", "5,5", "3,5", "3,3", "1,3"}, 15},
        test_data{{"1,1", "9,1", "9,5", "7,5", "7,3", "3,3", "3,5", "1,5"}, 21}
    );

    REQUIRE(day09::part2(item.rows) == item.expected);
}

