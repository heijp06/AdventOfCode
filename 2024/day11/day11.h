#pragma once

#include <map>
#include <string>
#include <vector>

namespace day11 {
    int64_t part1(const std::vector<std::string>& rows);
    int64_t part2(const std::vector<std::string>& rows);

    int64_t solve(const std::vector<std::string>& rows, int times = 25);
    std::map<int64_t, int64_t> parse(const std::string& row);
}
