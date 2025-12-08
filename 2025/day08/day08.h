#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day08 {
    struct coord3d_t {
        int x;
        int y;
        int z;
    };

    struct item_t {
        std::int64_t distance;
        coord3d p1;
        coord3d p2;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
