#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day11.h"

std::vector<std::string> rows{
    "aaa: you hhh",
    "you: bbb ccc",
    "bbb: ddd eee",
    "ccc: ddd eee fff",
    "ddd: ggg",
    "eee: out",
    "fff: out",
    "ggg: out",
    "hhh: ccc fff iii",
    "iii: out",
};

TEST_CASE("part1") {
    REQUIRE(day11::part1(rows) == 5);
}

TEST_CASE("part2") {
    REQUIRE(day11::part2(rows) == -1);
}
