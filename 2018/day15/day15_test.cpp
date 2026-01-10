#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day15.h"

const std::vector<std::vector<std::string>> grids = {
    {
        "#######",
        "#.G...#",
        "#...EG#",
        "#.#.#G#",
        "#..G#E#",
        "#.....#",
        "#######"},
    {
        "#######",
        "#G..#E#",
        "#E#E.E#",
        "#G.##.#",
        "#...#E#",
        "#...E.#",
        "#######"},
    {
        "#######",
        "#E..EG#",
        "#.#G.E#",
        "#E.##E#",
        "#G..#.#",
        "#..E#.#",
        "#######"},
    {
        "#######",
        "#E.G#.#",
        "#.#G..#",
        "#G.#.G#",
        "#G..#.#",
        "#...E.#",
        "#######"},
    {
        "#######",
        "#.E...#",
        "#.#..G#",
        "#.###.#",
        "#E#G#G#",
        "#...#G#",
        "#######"},
    {
        "#########",
        "#G......#",
        "#.E.#...#",
        "#..##..G#",
        "#...##..#",
        "#...#...#",
        "#.G...G.#",
        "#.....G.#",
        "#########"}
};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> rows;
        int outcome;
    };

    const auto& item = GENERATE(
        testdata{grids[0], 27730},
        testdata{grids[1], 36334},
        testdata{grids[2], 39514},
        testdata{grids[3], 27755},
        testdata{grids[4], 28944},
        testdata{grids[5], 18740}
    );

    REQUIRE(day15::part1(item.rows) == item.outcome);
}

TEST_CASE("part2") {
    struct testdata {
        std::vector<std::string> rows;
        int outcome;
    };

    const auto& item = GENERATE(
        testdata{grids[0], 4988},
        testdata{grids[2], 31284},
        testdata{grids[3], 3478},
        testdata{grids[4], 6474},
        testdata{grids[5], 1140}
    );

    REQUIRE(day15::part2(item.rows) == item.outcome);
}
