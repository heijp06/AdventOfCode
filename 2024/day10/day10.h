#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows);
    int64_t part2(const std::vector<std::string>& rows);

    int score(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord);
    int64_t rating(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord);
}
