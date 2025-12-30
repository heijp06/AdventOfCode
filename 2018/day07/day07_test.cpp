#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day07.h"

std::vector<std::string> rows{
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin."
};

TEST_CASE("part1") {
    REQUIRE(day07::part1(rows) == "CABDFE");
}

TEST_CASE("part2") {
    REQUIRE(day07::part2(rows, 2, 0) == 15);
}

TEST_CASE("solve.first") {
    REQUIRE(day07::solve(rows, 2, 0).first == "CABFDE");
}
