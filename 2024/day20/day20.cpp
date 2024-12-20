#include <map>

#include "day20.h"
#include "../../lib/advent.h"

namespace day20 {
    // 1374 too high.
    int part1(const std::vector<std::string>& rows, int min_cheat) {
        const auto& grid = advent::grid(rows);
        const auto& start = grid.find('S');
        const auto& end = grid.find('E');
        advent::coord position(start);
        auto time{0};
        std::map<advent::coord, int> timings{{start, 0}};
        std::vector<advent::direction> all_directions = {
            advent::direction::up(), advent::direction::right(),
            advent::direction::down(), advent::direction::left()};

        while (position != end) {
            time++;
            for (const auto& direction : all_directions) {
                const auto& new_position = position + direction;
                if (!timings.count(new_position) && grid[new_position] != '#') {
                    timings[new_position] = time;
                    position = new_position;
                    break;
                }
            }
        }

        auto result{0};
        for (const auto& [pos, timing] : timings) {
            for (const auto& direction : all_directions) {
                auto shortcut = pos + 2 * direction;
                if (timings.count(shortcut) && timings[shortcut] - timing >= min_cheat) {
                    result++;
                }
            }
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
