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

TEST_CASE("part2") {
    REQUIRE(day05::part2(rows) == 14);
}

TEST_CASE("merge") {
    struct test_data {
        std::vector<range_t> ranges;
        std::vector<range_t> merged;
    };

    const auto& cases = GENERATE(
        test_data{ {}, {} },
        test_data{ {{2, 3}}, {{2, 3}} },
        test_data{ {{2, 3}, {4, 5}}, {{2, 5}} },
        test_data{ {{2, 3}, {5, 6}}, {{2, 3}, {5, 6}} },
        test_data{ {{5, 6}, {2, 3}}, {{2, 3}, {5, 6}} },
        test_data{ {{2, 6}, {3, 5}}, {{2, 6}} }
    );

    std::vector<range_t> actual = day05::merge(cases.ranges);

    REQUIRE(actual == cases.merged);
}

