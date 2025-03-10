#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day05.h"

std::vector<std::string> rows = {
    "dabAcCaCBAcCcaDA"
};

TEST_CASE("part1") {
    REQUIRE(day05::part1(rows) == 10);
}
