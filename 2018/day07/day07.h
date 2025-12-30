#pragma once

#include <string>
#include <utility>
#include <vector>

namespace day07 {
    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows, int workers = 5, int delay = 60);

    std::pair<std::string, int> solve(const std::vector<std::string>& rows, int workers, int delay);
    std::vector<std::pair<char, char>> parse(const std::vector<std::string>& rows);
}
