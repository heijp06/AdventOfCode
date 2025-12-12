#pragma once

#include <iostream>
#include <iomanip>
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

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<Machine> parse(const std::vector<std::string>& rows);
    std::vector<Equation> parse_equations(const Machine& machine);
    std::vector<System> parse_systems(const std::vector<Machine>& machines);
    void dump(const System& system);
}
