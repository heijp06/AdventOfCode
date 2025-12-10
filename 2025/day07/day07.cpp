#include <algorithm>
#include <numeric>

#include "day07.h"
#include "../../lib/advent.h"

namespace day07 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        return solve(rows, false);
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        return solve(rows, true);
    }

    int64_t solve(const std::vector<std::string>& rows, bool part2) {
        const auto& manifold = advent::grid{rows};
        auto start = manifold.find('S').column;
        auto current_row = std::vector<std::int64_t>(manifold.get_width());
        auto next_row = current_row;
        current_row[start] = 1;
        std::int64_t split{};

        for (int row = 1; row < manifold.get_height(); row++) {
            for (int column = 0; column < manifold.get_width(); column++) {
                if (manifold[{row, column}] == '^') {
                    next_row[column - 1] += current_row[column];
                    next_row[column + 1] += current_row[column];
                    split++;
                }
                else {
                    next_row[column] += current_row[column];
                }

                current_row[column] = 0;
            }
            std::swap(current_row, next_row);
        }

        return part2 ? std::reduce(current_row.cbegin(), current_row.cend()) : split;
    }
}
