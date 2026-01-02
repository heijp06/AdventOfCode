#pragma once

#include <set>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day13 {
    struct Cart {
        advent::coord position;
        advent::direction direction;
    };

    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::set<Cart> parse_carts(advent::grid& grid);
}
