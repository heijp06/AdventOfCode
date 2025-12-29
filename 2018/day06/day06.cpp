#include <algorithm>

#include "day06.h"

namespace day06 {
    int part1(const std::vector<std::string>& rows) {
        int min_row{-1};
        int max_row{-1};
        int min_column{-1};
        int max_column{-1};

        const auto regions = parse(rows, min_column, max_column, min_row, max_row);

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<Region> parse(const std::vector<std::string>& rows, int& min_column, int& max_column, int& min_row, int& max_row) {
        std::vector<Region> regions{};
        regions.reserve(rows.size());

        for (const auto& row : rows) {
            const auto& fields = advent::ints(row);
            const auto row = fields[1];
            const auto column = fields[0];
            min_column = min_column == -1 ? column : std::min(min_column, column);
            max_column = max_column == -1 ? column : std::max(max_column, column);
            min_row = min_row == -1 ? row : std::min(min_row, row);
            max_row = max_row == -1 ? row : std::max(max_row, row);
            regions.push_back({{{row, column}}, {{row, column}}, false});
        }

        return regions;
    }
}
