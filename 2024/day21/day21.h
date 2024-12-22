#pragma once

#include <map>
#include <string>
#include <vector>

namespace day21 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::map<std::string, std::string> get_directional_moves();
}
