#include "day04.h"
#include "../../lib/advent.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows) {
        auto result{0};
        const auto& grid = advent::grid{rows};
        std::vector<advent::coord> directions{
            {-1, -1}, {-1, 0}, {-1, 1},
            {0, -1}, {0, 1},
            {1, -1}, {1, 0}, {1, 1},
        };

        for (const auto& current : grid.find_all('@')) {
            auto counter{0};
            for (const auto& direction : directions) {
                const auto& pos = current + direction;
                if (pos.column >= 0 && pos.column < grid.get_width()
                    && pos.row >= 0 && pos.row < grid.get_height()
                    && grid[pos] == '@') {
                    counter++;
                }
            }
            result += counter < 4;
        }
        
        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
