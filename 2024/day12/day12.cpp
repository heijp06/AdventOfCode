#include "day12.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows) {
        int height = rows.size();
        int width = rows[0].size();
        std::set<advent::coord> seen;
        std::vector<std::set<advent::coord>> regions;

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
                auto position = advent::coord{row, column};
                if (seen.count(position)) {
                    continue;
                }

                //TODO: maybe create set based on aray.
                const auto& region = create_region(rows, position);
                regions.push_back(region);
                seen.insert(region.cbegin(), region.cend());
            }
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::set<advent::coord> create_region(std::vector<std::string> rows, advent::coord position) {
        return std::set<advent::coord>();
    }
}
