#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day10 {
    struct Light {
        advent::coord position;
        advent::direction velocity;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
