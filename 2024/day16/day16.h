#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    struct reindeer {
        advent::coord position;
        advent::direction direction;

        friend bool operator==(const reindeer& left, const reindeer& right) {
            return left.position == right.position
                && left.direction == right.direction;
        }
        friend bool operator!=(const reindeer& left, const reindeer& right) {
            return !(left == right);
        }
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
}
