#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day14 {
    int part1(const std::vector<std::string>& rows, int width = 101, int height = 103);
    int part2(const std::vector<std::string>& rows, int width = 101, int height = 103);

    struct robot {
        advent::coord position;
        advent::coord velocity;
    };

    void output(const std::vector<robot>& robots, int width, int height);
}
