#include "day04.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows) {
        const auto& grid = advent::grid{rows};
        const std::vector<advent::coord> directions{
            {-1, -1}, {-1, 0}, {-1, 1},
            {0, -1}, {0, 1},
            {1, -1}, {1, 0}, {1, 1},
        };

        std::vector<advent::coord> result = get_accessible_rolls(grid, directions);
        
        return static_cast<int>(result.size());
    }

    int part2(const std::vector<std::string>& rows) {
        int result{0};
        auto grid = advent::grid{rows};
        const std::vector<advent::coord> directions{
            {-1, -1}, {-1, 0}, {-1, 1},
            {0, -1}, {0, 1},
            {1, -1}, {1, 0}, {1, 1},
        };

        std::vector<advent::coord> accessible = get_accessible_rolls(grid, directions);
        while (!accessible.empty())
        {
            result += static_cast<int>(accessible.size());
            for (const auto& roll : accessible) {
                grid[roll] = '.';
            }
            accessible = get_accessible_rolls(grid, directions);
        }
        
        return result;
    }

    std::vector<advent::coord> get_accessible_rolls(
        const advent::grid& grid, const std::vector<advent::coord>& directions)
    {
        std::vector<advent::coord> result;

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
            if (counter < 4)
            {
                result.push_back(current);
            }
        }
        return result;
    }
}
