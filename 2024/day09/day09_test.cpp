#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

std::vector<std::string> rows = {
    "2333133121414131402"
};

TEST_CASE("part1") {
    REQUIRE(day09::part1(rows) == 1928);
}
