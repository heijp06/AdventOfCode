#include <algorithm>
#include <string>

#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    std::string part1(const std::vector<std::string>& rows) {
        const auto serial_number = advent::ints(rows.front()).front();

        const auto& grid = create_grid(serial_number);

        return solve(grid).first;
    }

    std::string part2(const std::vector<std::string>& rows) {
        (void)rows;
        return "";
    }

    std::pair<std::string, int> solve(const std::vector<std::vector<int>>& grid) {
        int max{-300 * 300 * 5};
        int x_max = 0;
        int y_max = 0;

        for (int x = 1; x <= size - 2; x++) {
            for (int y = 1; y <= size - 2; y++) {
                int total{};
                for (int delta_x = 0; delta_x < 3; delta_x++) {
                    for (int delta_y = 0; delta_y < 3; delta_y++) {
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

        return {std::to_string(x_max) + "," + std::to_string(y_max), max};
    }

    std::vector<std::vector<int>> create_grid(const int serial_number) {
        std::vector<std::vector<int>> grid{size + 1, std::vector(size + 1, 0)};

        for (int x = 1; x <= size; x++) {
            int rack_id = x + 10;
            for (int y = 1; y <= size; y++) {
                int power_level = rack_id * y;
                power_level += serial_number;
                power_level *= rack_id;
                power_level %= 1000;
                power_level /= 100;
                power_level -= 5;
                grid[x][y] = power_level;
            }
        }

        return grid;
    }
}
