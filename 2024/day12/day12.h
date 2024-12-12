#pragma once

#include <set>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::set<advent::coord> create_region(const std::vector<std::string>& rows, const advent::coord& position);
	int perimeter(const std::set<advent::coord>& region);
    int sides(const std::set<advent::coord>& region);

    struct edge {
        advent::coord position;
        advent::coord outside;

        friend bool operator<(const edge& left, const edge& right) {
            return left.position == right.position ? left.outside < right.outside : left.position < right.position;
        }
        friend bool operator==(const edge& left, const edge& right) {
            return left.position == right.position && left.outside == right.outside;
        }
    };
}
