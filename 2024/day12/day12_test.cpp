#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day12.h"

std::vector<std::string> rows{
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
};

TEST_CASE("part1") {
    REQUIRE(day12::part1(rows) == 1930);
}

TEST_CASE("part2") {
    REQUIRE(day12::part2(rows) == 1206);
}

TEST_CASE("sides") {
    REQUIRE(day12::sides({{0, 0}}) == 4);
}
