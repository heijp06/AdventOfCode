#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day10.h"

std::vector<std::string> rows {
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732",
};

TEST_CASE("part1") {
    REQUIRE(day10::part1(rows) == 36);
}
