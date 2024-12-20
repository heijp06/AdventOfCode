#pragma once

#include <string>
#include <vector>

namespace day20 {
    int part1(const std::vector<std::string>& rows, int min_cheat = 100);
    size_t part2(const std::vector<std::string>& rows, int min_cheat = 100);

    size_t solve(const std::vector<std::string>& rows, int min_cheat, int cheat_length);
}
