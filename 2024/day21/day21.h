#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day21 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    int64_t solve(const std::vector<std::string>& rows, int times);
    std::vector<std::map<std::string, std::string>> get_directional_moves();
    std::vector<std::string> get_numerical_moves(const char from, const char to);
    advent::coord get_position(const char c);
    std::int64_t get_moves(const std::string& move, const std::map<std::string, std::string>& mapping, int count);
}
