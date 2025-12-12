#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day10.h"

std::vector<std::string> rows{
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
};

TEST_CASE("part1") {
    REQUIRE(day10::part1(rows) == 7);
}

TEST_CASE("extra") {
    REQUIRE(day10::part1({"[##.#] (1,2) (0,2) (2,3) (0,3) (0,1,2) {24,4,29,37}"}) == 2);
}

TEST_CASE("part2") {
    REQUIRE(day10::part2(rows) == 33);
}
