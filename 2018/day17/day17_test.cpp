#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day17.h"

std::vector<std::string> rows{
    "x=495, y=2..7",
    "y=7, x=495..501",
    "x=501, y=3..7",
    "x=498, y=2..4",
    "x=506, y=1..2",
    "x=498, y=10..13",
    "x=504, y=10..13",
    "y=13, x=498..504"
};

TEST_CASE("part1") {
    REQUIRE(day17::part1(rows) == 57);
}

TEST_CASE("part2") {
    REQUIRE(day17::part2(rows) == -1);
}

TEST_CASE("extra") {
    struct testdata {
        std::vector<std::string> rows;
        int water;
    };

    const auto& item = GENERATE(
        testdata{
            {
                ".....+.....",
                "...........",
                ".#.......#.",
                ".#..###..#.",
                ".#..#.#..#.",
                ".#..###..#.",
                ".#.......#.",
                ".#########."
            }, 49},
        testdata{
            {
                "...+...",
                ".......",
                "...#...",
                ".......",
                ".#...#.",
                ".#####.",
            }, 16}
        );

    REQUIRE(day17::solve(advent::grid(item.rows)) == item.water);
}
