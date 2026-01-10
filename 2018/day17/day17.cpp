#include <algorithm>
#include <iostream>
#include <utility>

#include "day17.h"

namespace day17 {
    int part1(const std::vector<std::string>& rows) {
        auto grid = parse(rows);

        std::cout << std::endl;
        grid.draw();

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    advent::grid parse(const std::vector<std::string>& rows) {
        auto veins = std::vector<std::pair<char, std::vector<int>>>{};
        veins.reserve(rows.size());

        auto min_x{500};
        auto max_x{500};
        int max_y{};

        for (const auto& row : rows) {
            char coordinate = row[0];
            const auto& ints = advent::ints(row);
            veins.push_back({coordinate, ints});
            if (coordinate == 'x') {
                min_x = std::min(min_x, ints[0]);
                max_x = std::max(max_x, ints[0]);
                max_y = std::max(max_y, ints[2]);
            }
            else {
                min_x = std::min(min_x, ints[1]);
                max_x = std::max(max_x, ints[2]);
                max_y = std::max(max_y, ints[0]);
            }
        }

        advent::grid grid{max_y + 1, max_x - min_x + 3};
        int delta = min_x - 1;
        grid[{0, 500 - delta}] = '+';

        for (const auto& vein : veins) {
            int z = vein.second[0];
            int start = vein.second[1];
            int end = vein.second[2];
            char coordinate = vein.first;

            for (int i = start; i <= end; i++) {
                if (coordinate == 'x') {
                    grid[{i, z - delta}] = '#';
                }
                else {
                    grid[{z, i - delta}] = '#';
                }
            }
        }

        return grid;
    }
}
