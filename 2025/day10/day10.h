#pragma once

#include <iostream>
#include <iomanip>
#include <set>
#include <string>
#include <vector>

namespace day10 {
    struct Machine {
        int lamps;
        int diagram;
        std::vector<int> buttons;
        std::vector<int> joltages;
    };

    struct Equation {
        std::vector<int> coefficients;
        int value = 0;
    };

    struct System {
        std::vector<int> upper_bounds;
        std::vector<Equation> equations;
    };

    struct Solution {
        std::vector<int> values;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<Machine> parse(const std::vector<std::string>& rows);
    std::vector<Equation> parse_equations(const Machine& machine);
    std::vector<System> parse_systems(const std::vector<Machine>& machines);
    std::vector<Solution> solve(const System& system);
    System reduce(const System& system, const int row_index);
    bool check(const System& system, const Solution& solution);
    void dump(const System& system);
}
