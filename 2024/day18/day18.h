#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day18 {
    struct state {
        int cost;
        advent::coord position;

        friend bool operator<(const state& left, const state& right) {
            if (left.cost != right.cost) {
                return left.cost < right.cost;
            }
            return left.position < right.position;
        }
    };

    int part1(const std::vector<std::string>& rows, int size = 70, int nanoseconds = 1024);
    int part2(const std::vector<std::string>& rows);

    int heuristic(const state& s, int size);
}
