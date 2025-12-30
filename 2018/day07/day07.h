#pragma once

#include <string>
#include <utility>
#include <vector>

namespace day07 {
    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::pair<char, char>> parse(const std::vector<std::string>& rows);
}
