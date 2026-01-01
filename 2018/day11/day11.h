#pragma once

#include <string>
#include <vector>

namespace day11 {
    const size_t size = 300;

    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::vector<int>> create_grid(const int serial_number);
}
