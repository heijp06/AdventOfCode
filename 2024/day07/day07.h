#pragma once

#include <string>
#include <vector>

namespace day07 {
    int64_t part1(const std::vector<std::string>& rows);
    int64_t part2(const std::vector<std::string>& rows);

    bool solve(const std::vector<int64_t>& equation, bool part2 = false);
    int64_t concatenate(const int64_t left, const int64_t right);
}
