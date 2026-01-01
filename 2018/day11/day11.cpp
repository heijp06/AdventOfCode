#include <algorithm>
#include <string>

#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    std::string part1(const std::vector<std::string>& rows) {
        const auto& grid = create_grid(rows);

        return solve(grid, 3).first;
    }

    std::string part2(const std::vector<std::string>& rows) {
        const auto& grid = create_grid(rows);

        std::pair<std::string, int> max{};
        for (int block_size = 1; block_size <= size; block_size++) {
            const auto& solution = solve(grid, block_size);
            if (max.second < solution.second) {
                max = {solution.first + "," + std::to_string(block_size), solution.second};
            }
        }

        return max.first;
    }

    std::pair<std::string, int> solve(const std::vector<std::vector<int>>& grid, int block_size) {
        int max{-300 * 300 * 5};
        int x_max = 0;
        int y_max = 0;

        for (int x = 1; x <= size - (block_size - 1); x++) {
            for (int y = 1; y <= size - (block_size - 1); y++) {
                int total =
                    grid[x + block_size - 1][y + block_size - 1]
                    - grid[x - 1][y + block_size - 1] - grid[x + block_size - 1][y - 1]
                    + grid[x - 1][y - 1];
                if (total > max) {
                    max = total;
                    x_max = x;
                    y_max = y;
                }
            }
        }

        return {std::to_string(x_max) + "," + std::to_string(y_max), max};
    }

    std::vector<std::vector<int>> create_grid(const std::vector<std::string>& rows) {
        // A cell (x, y) in the grid contains the sum of all power levels in the top left rectangle
        // from (1, 1) to (x, y). This is somewhat complex but allows calculating sums of power levels in blocks fast.
        const auto serial_number = advent::ints(rows.front()).front();
        std::vector<std::vector<int>> grid{size + 1, std::vector(size + 1, 0)};

        for (int y = 1; y <= size; y++) {
            for (int x = 1; x <= size; x++) {
                int rack_id = x + 10;
                int power_level = rack_id * y;
                power_level += serial_number;
                power_level *= rack_id;
                power_level %= 1000;
                power_level /= 100;
                power_level -= 5;
                grid[x][y] = grid[x - 1][y] + grid[x][y - 1] - grid[x - 1][y - 1] + power_level;
            }
        }

        return grid;
    }
}
