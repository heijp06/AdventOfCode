#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day13 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int64_t solve(const advent::coord& a, const advent::coord& b, const advent::coord& prize);
}
