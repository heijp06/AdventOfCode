#include "day14.h"
#include "../../lib/advent.h"

namespace day14 {
    int part1(const std::vector<std::string>& rows, int width, int height) {
        int q1 = 0;
        int q2 = 0;
        int q3 = 0;
        int q4 = 0;

        for (const auto& row : rows) {
            const auto& data = advent::ints(row);
            auto x0 = data[0];
            auto y0 = data[1];
            auto vx = data[2];
            auto vy = data[3];

            auto x = (x0 + vx * 100) % width;
            if (x < 0) {
                x += width;
            }
            auto y = (y0 + vy * 100) % height;
            if (y < 0) {
                y += height;
            }

            if (x < (width - 1) / 2) {
                q1 += y < (height - 1) / 2;
                q3 += y > (height - 1) / 2;
            }
            else if (x > (width - 1) / 2) {
                q2 += y < (height - 1) / 2;
                q4 += y > (height - 1) / 2;
            }
        }

        return q1 * q2 * q3 * q4;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
