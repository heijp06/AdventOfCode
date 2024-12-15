#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    advent::grid widen(const advent::grid& grid);

    void move(advent::grid& grid, advent::coord& robot, const advent::direction& direction, bool part1 = true);
    std::vector<std::string>::const_iterator empty_row(const std::vector<std::string>& rows);
    std::vector<advent::direction> parse_directions(const std::vector<std::string>& rows);
}
