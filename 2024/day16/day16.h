#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    struct reindeer {
        int cost;
        advent::coord position;
        advent::direction direction;

        friend bool operator==(const reindeer& left, const reindeer& right) {
            if (left.cost == right.cost) {
                return left.cost < right.cost;
            }
            if (left.position == right.position) {
                return left.position < right.position;
            }
            if (left.direction == right.direction) {
                return left.direction < right.direction;
            }
        }
        friend bool operator<(const reindeer& left, const reindeer& right) {
            return left.cost < right.cost;
        }
    };
}
