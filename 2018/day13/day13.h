#pragma once

#include <map>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day13 {
    enum Turn { Left, Straight, Right };

    struct Cart {
        advent::coord position;
        advent::direction direction;
        Turn turn;

        Cart move(const advent::grid& grid) const;
    };

    std::string part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::map<advent::coord, Cart> parse_carts(advent::grid& grid);
    void draw(advent::grid grid, const std::map<advent::coord, Cart>& carts);
}
