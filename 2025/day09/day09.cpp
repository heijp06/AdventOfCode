#include <cmath>
#include <utility>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t max_area{};
        const auto& pairs = parse(rows);

        for (const auto& pair1 : pairs) {
            for (const auto& pair2 : pairs) {
                auto area =
                    (std::abs(pair2.first - pair1.first) + 1) * (std::abs(pair2.second - pair1.second) + 1);
                max_area = std::max(max_area, area);
            }
        }
        
        return max_area;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        auto& pairs = parse(rows);
        std::int64_t max_x{};
        std::int64_t max_y{};
        for (const auto& pair : pairs) {
            max_x = std::max(max_x, pair.first);
            max_y = std::max(max_y, pair.second);
        }

        auto grid = advent::grid(static_cast<int>(max_x + 1), static_cast<int>(max_y + 1));

        const auto& current = pairs.back();
        grid[{static_cast<int>(current.first), static_cast<int>(current.second)}] = 'O';
        for (const auto& pair : pairs) {
            
        }
        ////grid.draw();

        return -1;
    }

    std::vector<std::pair<int64_t, int64_t>> parse(const std::vector<std::string>& rows) {
        std::vector<std::pair<int64_t, int64_t>> pairs;
        pairs.reserve(rows.size());

        for (const auto& row : rows) {
            const auto& ints = advent::ints<int64_t>(row);
            pairs.emplace_back(std::make_pair(ints[0], ints[1]));
        }

        return pairs;
    }
}
