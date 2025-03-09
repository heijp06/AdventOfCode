#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day04.h"

std::vector<std::string> rows = {
    "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-01 00:25] wakes up",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-02 00:50] wakes up",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-05 00:55] wakes up"
};

TEST_CASE("part1") {
    REQUIRE(day04::part1(rows) == -1);
}

TEST_CASE("start at midnight") {
    struct test_data {
        std::string line;
        std::string expected;
    };

    auto item = GENERATE(
        test_data{"[1518-11-01 00:55] wakes up", "[1518-11-01 00:55] wakes up"},
        test_data{"[1518-11-01 23:58] Guard #99 begins shift", "[1518-11-02 00:00] Guard #99 begins shift"},
        test_data{"[1518-11-09 23:58] Guard #99 begins shift", "[1518-11-10 00:00] Guard #99 begins shift"},
        test_data{"[1518-11-19 23:58] Guard #99 begins shift", "[1518-11-20 00:00] Guard #99 begins shift"},
        test_data{"[1518-05-31 23:58] Guard #99 begins shift", "[1518-06-01 00:00] Guard #99 begins shift"}
        );

    REQUIRE(day04::start_at_midnight(item.line) == item.expected);
}
