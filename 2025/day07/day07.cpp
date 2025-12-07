#include <algorithm>
#include <numeric>

#include "day07.h"
#include "../../lib/advent.h"

namespace day07 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        const auto& manifold = advent::grid{rows};
        auto start = manifold.find('S').column;
        auto current_row = std::vector<bool>(manifold.get_width());
        current_row[start] = true;
        auto next_row = std::vector<bool>(manifold.get_width());
        int split{};

        for (int row = 1; row < manifold.get_height(); row++) {
            for (int column = 0; column < manifold.get_width(); column++) {
                if (!current_row[column]) {
                    continue;
                }

                if (manifold[{row, column}] == '^') {
                    next_row[column - 1] = true;
                    next_row[column + 1] = true;
                    split++;
                }
                else {
                    next_row[column] = true;
                }
            }
            std::swap(current_row, next_row);
            std::fill(next_row.begin(), next_row.end(), false);
        }
        
        return split;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        const auto& manifold = advent::grid{rows};
        auto start = manifold.find('S').column;
        auto current_row = std::vector<std::int64_t>(manifold.get_width());
        current_row[start] = true;
        auto next_row = std::vector<std::int64_t>(manifold.get_width());

        for (int row = 1; row < manifold.get_height(); row++) {
            for (int column = 0; column < manifold.get_width(); column++) {
                if (!current_row[column]) {
                    continue;
                }

                if (manifold[{row, column}] == '^') {
                    next_row[column - 1] += current_row[column];
                    next_row[column + 1] += current_row[column];
                }
                else {
                    next_row[column] += current_row[column];
                }
            }
            std::swap(current_row, next_row);
            std::fill(next_row.begin(), next_row.end(), 0);
        }
        
        return std::reduce(current_row.cbegin(), current_row.cend());
    }
}
