#pragma once

#include <set>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::set<advent::coord> create_region(std::vector<std::string> rows, advent::coord position);
}
