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
    struct testdata {
        std::vector<std::string> rows;
        size_t count;
    };

    const auto& item = GENERATE(
        testdata{{"01245"}, 5},
        testdata{{"51589"}, 9},
        testdata{{"92510"}, 18},
        testdata{{"59414"}, 2018}
    );

    REQUIRE(day14::part2(item.rows) == item.count);
}
