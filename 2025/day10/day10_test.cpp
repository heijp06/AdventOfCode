#include <set>
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

TEST_CASE("part2") {
    REQUIRE(day10::part2(rows) == 33);
}

TEST_CASE("extra") {
    REQUIRE(day10::part1({"[##.#] (1,2) (0,2) (2,3) (0,3) (0,1,2) {24,4,29,37}"}) == 2);
}

TEST_CASE("test system that gives the wrong solution") {
    REQUIRE(day10::part2({"[####] (1,2) (0,3) (0,1,2) {20,20,20,1}"}) == 21);
}

/*

  21  20  32  20  21
   1   1   0   0   1   |  30
   1   0   0   0   1   |  21
   1   0   1   0   0   |  32
   0   1   0   1   0   |  20

ERR:   21   9  11   0   0

*/

TEST_CASE("1 row 1 element 1 solution") {
    day10::System system = {
        {3},
        {{{ 2 }, 4 }}
    };

    const auto& solutions = day10::solve(system);

    REQUIRE(solutions.size() == 1);
    REQUIRE(solutions.front().values == std::vector<int>{2});
}

TEST_CASE("1 row 1 element no solution") {
    day10::System system = {
        {3},
        {{{ 2 }, 5 }}
    };

    const auto& solutions = day10::solve(system);

    REQUIRE(solutions.empty());
}

TEST_CASE("1 row 2 elements 1 solution") {
    day10::System system = {
        {2, 2},
        {{{ 2, 3 }, 5 }}
    };

    const auto& solutions = day10::solve(system);

    REQUIRE(solutions.size() == 1);
    REQUIRE(solutions.front().values == std::vector<int>{{1, 1}});
}

TEST_CASE("2 row 2 elements 1 solution") {
    day10::System system = {
        {4, 5}, {
            {{ 2, 3 }, 18 },
            {{ -1, 1}, 1}
        }
    };

    const auto& solutions = day10::solve(system);

    REQUIRE(solutions.size() == 1);
    REQUIRE(solutions.front().values == std::vector<int>{{3, 4}});
}

TEST_CASE("reduce system") {
    day10::System system = {
        {1, 2, 3}, {
            {{0, 2, 3}, 6},
            {{3, 0, 2}, 4},
            {{2, 1, 0}, 3}
    }};

    day10::System expected = {
        {2, 3}, {
            {{2, 3}, 6},
            {{3, -4}, 1}
    }};

    const auto& actual = day10::reduce(system, 1);

    REQUIRE(actual.upper_bounds == expected.upper_bounds);
    REQUIRE(actual.equations.size() == expected.equations.size());
    REQUIRE(actual.equations[0].coefficients == expected.equations[0].coefficients);
    REQUIRE(actual.equations[0].value == expected.equations[0].value);
    REQUIRE(actual.equations[1].coefficients == expected.equations[1].coefficients);
    REQUIRE(actual.equations[1].value == expected.equations[1].value);
}
