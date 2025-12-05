#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "day05.h"

using range_t = std::pair<std::int64_t, std::int64_t>;

namespace day05 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    void parse(const std::vector<std::string>& rows, std::vector<int64_t>& ingredients, std::vector<range_t>& ranges);
    void merge(const range_t& range, std::vector<range_t>& ranges);
}
