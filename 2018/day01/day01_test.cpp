#include <vector>
#include <string>

#include "../lib/catch.hpp"
#include "day01.h"

TEST_CASE("changes")
{
    std::vector<std::string> rows = {"-1", "0", "+1"};
    std::vector<int> expected = {-1, 0, 1};

    std::vector<int> actual = day01::changes(rows);

    REQUIRE(actual == expected);
}
