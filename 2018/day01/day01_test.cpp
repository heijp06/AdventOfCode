#include <vector>
#include <string>

#include "../lib/catch.hpp"
#include "day01.h"

struct example {
    std::vector<std::string> rows;
    int twice;
};

TEST_CASE("changes") {
    const std::vector<std::string>& rows = {"-1", "0", "+1"};
    const std::vector<int>& expected = {-1, 0, 1};

    const std::vector<int>& actual = day01::changes(rows);

    REQUIRE(actual == expected);
}

TEST_CASE("part2") {
    const auto& item = GENERATE(
        example{{"+1", "-1"}, 0},
        example{{"+3", "+3", "+4", "-2", "-4"}, 10},
        example{{"-6", "+3", "+8", "+5", "-6"}, 5},
        example{{"+7", "+7", "-2", "-7", "-4"}, 14}
    );

    const auto& actual = day01::part2(item.rows);

    REQUIRE(actual == item.twice);
}
