#include <vector>
#include <string>
#include <utility>

#include "../lib/catch.hpp"
#include "day01.h"

TEST_CASE("changes") {
    std::vector<std::string> rows = {"-1", "0", "+1"};
    std::vector<int> expected = {-1, 0, 1};

    std::vector<int> actual = day01::changes(rows);

    REQUIRE(actual == expected);
}

TEST_CASE("part2") {
	using pair = std::pair<std::vector<std::string>, int>;

    const auto& p = GENERATE(
        pair{{"+1", "-1"}, 0},
        pair{{"+3", "+3", "+4", "-2", "-4"}, 10},
        pair{{"-6", "+3", "+8", "+5", "-6"}, 5},
        pair{{"+7", "+7", "-2", "-7", "-4"}, 14}
    );

    auto actual = day01::part2(p.first);

    REQUIRE(actual == p.second);
}
