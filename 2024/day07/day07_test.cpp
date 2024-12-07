#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day07.h"

struct test_data {
    int64_t left;
    int64_t right;
    int64_t result;
};

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

TEST_CASE("part2") {
    REQUIRE(day07::part2(rows) == 11387);
}

TEST_CASE("concatenate") {
    const auto& item = GENERATE(
        test_data{1, 2, 12},
        test_data{9, 2, 92},
        test_data{10, 2, 102},
        test_data{11, 2, 112},
        test_data{95914368903131, 128, 95914368903131128}
    );

    REQUIRE(day07::concatenate(item.left, item.right) == item.result);
}

TEST_CASE("do_not_multiply_by_zero") {
    // Previous code used an initial value of 0 and then used that in multiplication which is wrong.
    REQUIRE_FALSE(day07::solve({0, 1}));
}
