#pragma once

#include <set>
#include <string>
#include <utility>
#include <vector>

namespace day05 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    void parse(const std::vector<std::string>& rows, std::set<std::pair<int, int>>& rules, std::vector<std::vector<int>>& updates);
}
