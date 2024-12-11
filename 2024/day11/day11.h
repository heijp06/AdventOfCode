#pragma once

#include <map>
#include <string>
#include <vector>

namespace day11 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::map<int64_t, int> parse(const std::string& row);
}
