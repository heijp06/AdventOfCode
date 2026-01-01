#include <algorithm>
#include <string>

#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    std::string part1(const std::vector<std::string>& rows) {
        const int size = 300;
        const auto serial_number = advent::ints(rows.front()).front();
        std::vector<std::vector<int>> grid{size + 1, std::vector(size + 1, 0)};

        for (size_t x = 1; x <= size; x++) {
            int rack_id = x + 10;
            for (size_t y = 1; y <= size; y++) {
                int power_level = rack_id * y;
                power_level += serial_number;
                power_level *= rack_id;
                power_level %= 1000;
                power_level /= 100;
                power_level -= 5;
                grid[x][y] = power_level;
            }
        }

        int max{-90};
        int x_max = 0;
        int y_max = 0;
        for (size_t x = 1; x <= size - 2; x++) {
            for (size_t y = 1; y <= size - 2; y++) {
                int total{};
                for (size_t delta_x = 0; delta_x < 3; delta_x++) {
                    for (size_t delta_y = 0; delta_y < 3; delta_y++) {
                        total += grid[x + delta_x][y + delta_y];
                    }
                }
                if (total > max) {
                    max = total;
                    x_max = x;
                    y_max = y;
                }
            }
        }

        return std::to_string(x_max) + "," + std::to_string(y_max);
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
