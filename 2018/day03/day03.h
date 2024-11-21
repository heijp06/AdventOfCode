#pragma once

#include <string>
#include <vector>

namespace day03 {
    struct claim {
        int id;
        int left;
        int top;
        int width;
        int height;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
    claim parse(std::string row);
}
