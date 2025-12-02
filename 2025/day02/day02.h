#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day02 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::int64_t sum_invalid(const std::int64_t& num_start, const std::int64_t& num_end);
}
