#pragma once

#include <string>
#include <vector>

namespace day11 {
    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::vector<int>> create_grid(const size_t size, const int serial_number);
}
