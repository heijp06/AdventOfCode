#include "../../lib/catch.hpp"
#include "day__NN__.h"

#include <string>
#include <vector>

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day__NN__::part1(rows) == -1);
}
