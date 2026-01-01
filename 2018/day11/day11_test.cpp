#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day11.h"

std::vector<std::string> rows{

};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> rows;
        std::string result;
    };

    const auto& item = GENERATE(
        testdata{{"18"}, "33,45"},
        testdata{{"42"}, "21,61"}
    );

    REQUIRE(day11::part1(item.rows) == item.result);
}

TEST_CASE("part2") {
    struct testdata {
        std::vector<std::string> rows;
        std::string result;
    };

    const auto& item = GENERATE(
        testdata{{"18"}, "90,269,16"},
        testdata{{"42"}, "232,251,12"}
    );

    REQUIRE(day11::part2(item.rows) == item.result);
}
