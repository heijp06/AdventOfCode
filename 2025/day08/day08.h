#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day08 {
    struct coord3d_t {
        std::int64_t x;
        std::int64_t y;
        std::int64_t z;

        friend bool operator<(const coord3d_t& l, const coord3d_t& r) {
            if (l.x < r.x) return true;
            if (l.x > r.x) return false;
            if (l.y < r.y) return true;
            if (l.y > r.y) return false;
            return l.z < r.z;
        }

        friend bool operator>(const coord3d_t& l, const coord3d_t& r) {
            if (l.x < r.x) return false;
            if (l.x > r.x) return true;
            if (l.y < r.y) return false;
            if (l.y > r.y) return true;
            return l.z > r.z;
        }
    };

    struct item_t {
        std::int64_t distance;
        coord3d_t p1;
        coord3d_t p2;

        friend bool operator<(const item_t& l, const item_t& r) {
            if (l.distance < r.distance) return true;
            if (l.distance > r.distance) return false;
            if (l.p1 < r.p1) return true;
            if (l.p1 > r.p1) return false;
            return l.p2 < r.p2;
        }
    };

    int part1(const std::vector<std::string>& rows, int times = 1000);
    int part2(const std::vector<std::string>& rows);

    int solve(const std::vector<std::string>& rows, int times);
}
