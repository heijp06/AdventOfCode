#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day06 {
    struct Region {
        std::vector<advent::coord> area;
        std::vector<advent::coord> edge;
        bool infinite;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<Region> parse(const std::vector<std::string>& rows, int& min_column, int& max_column, int& min_row, int& max_row);
}
