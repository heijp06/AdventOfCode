#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day15.h"

std::vector<std::string> rows{

};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> rows;
        int outcome;
    };

    const auto& item = GENERATE(
        testdata{{
            "#######",
            "#.G...#",
            "#...EG#",
            "#.#.#G#",
            "#..G#E#",
            "#.....#",
            "#######"}, 27730}
            );

    REQUIRE(day15::part1(item.rows) == item.outcome);
}

TEST_CASE("part2") {
    REQUIRE(day15::part2(rows) == -1);
}
