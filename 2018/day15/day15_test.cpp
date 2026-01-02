#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day15.h"

std::vector<std::string> rows{

};

TEST_CASE("part1") {
    struct testdata {
        std::vector<std::string> rows;
        int outcome;
    };

    const auto& item = GENERATE(
        testdata{{
            "#######",
            "#.G...#",
            "#...EG#",
            "#.#.#G#",
            "#..G#E#",
            "#.....#",
            "#######"}, 27730},
        testdata{{
            "#######",
            "#G..#E#",
            "#E#E.E#",
            "#G.##.#",
            "#...#E#",
            "#...E.#",
            "#######"}, 36334},
        testdata{{
            "#######",
            "#E..EG#",
            "#.#G.E#",
            "#E.##E#",
            "#G..#.#",
            "#..E#.#",
            "#######"}, 39514},
        testdata{{
            "#######",
            "#E.G#.#",
            "#.#G..#",
            "#G.#.G#",
            "#G..#.#",
            "#...E.#",
            "#######"}, 27755},
        testdata{{
            "#######",
            "#.E...#",
            "#.#..G#",
            "#.###.#",
            "#E#G#G#",
            "#...#G#",
            "#######"}, 28944},
        testdata{{
            "#########",
            "#G......#",
            "#.E.#...#",
            "#..##..G#",
            "#...##..#",
            "#...#...#",
            "#.G...G.#",
            "#.....G.#",
            "#########"}, 18740}
        );

    REQUIRE(day15::part1(item.rows) == item.outcome);
}

TEST_CASE("part2") {
    REQUIRE(day15::part2(rows) == -1);
}
