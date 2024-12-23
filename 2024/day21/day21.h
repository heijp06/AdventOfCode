#pragma once

#include <map>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day21 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::map<std::string, std::string>> get_directional_moves();
    std::vector<std::string> get_numerical_moves(const char from, const char to);
    advent::coord get_position(const char c);
}
