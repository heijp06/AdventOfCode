#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day09 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::vector<std::pair<int64_t, int64_t>> parse(const std::vector<std::string>& rows);
}
