#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::string>::const_iterator empty_row(const std::vector<std::string>& rows);
    std::vector<advent::direction> parse_directions(const std::vector<std::string>& rows);
}
