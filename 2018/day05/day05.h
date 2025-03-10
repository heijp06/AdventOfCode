#pragma once

#include <string>
#include <vector>

namespace day05 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int solve(const std::string& polymer, const char skip = ' ');
}
