#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int solve(const std::vector<std::string>& rows, bool part2 = false);
    int count(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord, bool part2 = false);
}
