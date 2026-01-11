#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day17 {
    int part1(const std::vector<std::string>& rows);
    int solve(advent::grid& grid);
    int part2(const std::vector<std::string>& rows);

    advent::grid parse(const std::vector<std::string>& rows);
}
