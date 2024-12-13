#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day13 {
    int64_t part1(const std::vector<std::string>& rows);
    int64_t part2(const std::vector<std::string>& rows);

    int64_t solve(const std::vector<std::string>& rows, bool part2 = false);
    int64_t solve_equations(const advent::coord& a, const advent::coord& b, const advent::coord& prize, bool part2);
}
