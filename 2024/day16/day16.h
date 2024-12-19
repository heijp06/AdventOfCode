#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day16 {
    struct reindeer {
        advent::coord position;
        advent::direction direction;
        std::vector<advent::coord> trail;

        friend bool operator<(const reindeer& left, const reindeer& right) {
            if (left.position != right.position) {
                return left.position < right.position;
            }
            return left.direction < right.direction;
        }
        friend bool operator>(const reindeer& left, const reindeer& right) {
            if (left.position != right.position) {
                return left.position > right.position;
            }
            return left.direction > right.direction;
        }
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int calculate_heuristic(const reindeer& state, const advent::coord& end);
    int turn_heuristic(const reindeer& state, const advent::coord& end);
    int move_heuristic(const reindeer& state, const advent::coord& end);
}
