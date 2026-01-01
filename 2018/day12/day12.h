#pragma once

#include <cstdint>
#include <set>
#include <string>
#include <vector>

namespace day12 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::int64_t solve(const std::vector<std::string>& rows, bool part2);
    std::vector<bool> parse_clues(const std::vector<std::string>& rows);
    std::set<std::int64_t> parse_initial_state(const std::vector<std::string>& rows);
}
