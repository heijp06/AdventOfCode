#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day12.h"

std::vector<std::string> rows{
    "initial state: #..#.#..##......###...###",
    "",
    "...## => #",
    "..#.. => #",
    ".#... => #",
    ".#.#. => #",
    ".#.## => #",
    ".##.. => #",
    ".#### => #",
    "#.#.# => #",
    "#.### => #",
    "##.#. => #",
    "##.## => #",
    "###.. => #",
    "###.# => #",
    "####. => #"
};

TEST_CASE("part1") {
    REQUIRE(day12::part1(rows) == 325);
}
