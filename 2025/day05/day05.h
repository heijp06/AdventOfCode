#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "day05.h"

using range_t = std::pair<std::int64_t, std::int64_t>;

namespace day05 {
    int part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    void parse(const std::vector<std::string>& rows, std::vector<int64_t>& ingredients, std::vector<range_t>& ranges);
    std::vector<range_t> merge(const std::vector<range_t>& ranges);
}
