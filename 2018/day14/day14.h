#pragma once

#include <string>
#include <utility>
#include <vector>

namespace day14 {
    std::string part1(const std::vector<std::string>& rows);
    size_t part2(const std::vector<std::string>& rows);

    std::pair<std::string, size_t> solve(const std::vector<std::string>& rows, bool part2);
    size_t check(const std::string& row, const std::vector<int>& scores);
}
