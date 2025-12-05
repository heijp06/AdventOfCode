#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<advent::coord> get_accessible_rolls(
        const advent::grid& grid, const std::vector<advent::coord>& directions);
}
