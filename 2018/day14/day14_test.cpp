#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day14.h"

std::vector<std::string> rows{

};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> rows;
        std::string scores;
    };

    const auto& item = GENERATE(
        testdata{{"5"}, "0124515891"},
        testdata{{"9"}, "5158916779"},
        testdata{{"18"}, "9251071085"},
        testdata{{"2018"}, "5941429882"}
    );

    REQUIRE(day14::part1(item.rows) == item.scores);
}

TEST_CASE("part2") {
    REQUIRE(day14::part2(rows) == -1);
}
