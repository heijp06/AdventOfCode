#include <vector>
#include <string>

#include "../lib/catch.hpp"
#include "day01.h"

TEST_CASE("changes") {
    std::vector<std::string> rows = {"-1", "0", "+1"};
    std::vector<int> expected = {-1, 0, 1};

    std::vector<int> actual = day01::changes(rows);

    REQUIRE(actual == expected);
}

TEST_CASE("part2(+7, +7, -2, -7, -4) == 14") {
    std::vector<std::string> rows = {"+7", "+7", "-2", "-7", "-4"};
    auto expected = 14;

    auto actual = day01::part2(rows);

    REQUIRE(actual == expected);
}
