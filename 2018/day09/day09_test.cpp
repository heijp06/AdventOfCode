#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

std::vector<std::string> rows{

};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> game;
        int score;
    };

    const auto& item = GENERATE(
        testdata{{"9 players; last marble is worth 25 points"}, 32},
        testdata{{"10 players; last marble is worth 1618 points"}, 8317},
        testdata{{"13 players; last marble is worth 7999 points"}, 146373},
        testdata{{"17 players; last marble is worth 1104 points"}, 2764},
        testdata{{"21 players; last marble is worth 6111 points"}, 54718},
        testdata{{"30 players; last marble is worth 5807 points"}, 37305}
    );

    REQUIRE(day09::part1(item.game) == item.score);
}

TEST_CASE("part2") {
    REQUIRE(day09::part2(rows) == -1);
}
