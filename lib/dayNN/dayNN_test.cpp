#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day__NN__.h"

std::vector<std::string> rows;

TEST_CASE("part1") {
    REQUIRE(day__NN__::part1(rows) == -1);
}
