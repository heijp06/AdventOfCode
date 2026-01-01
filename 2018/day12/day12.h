#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day12 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::int64_t solve(const std::vector<std::string>& rows, bool part2);
}
