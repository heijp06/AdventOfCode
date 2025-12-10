#pragma once

#include <string>
#include <vector>

namespace day10 {
    struct Machine {
        int lamps;
        int diagram;
        std::vector<int> buttons;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<Machine> parse(const std::vector<std::string>& rows);
}
