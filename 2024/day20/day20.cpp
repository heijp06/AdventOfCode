#include <cmath>
#include <map>
#include <set>
#include <utility>

#include "day20.h"
#include "../../lib/advent.h"

namespace day20 {
    size_t part1(const std::vector<std::string>& rows, int min_cheat) {
        return solve(rows, min_cheat, 2);
    }

    size_t part2(const std::vector<std::string>& rows, int min_cheat) {
        return solve(rows, min_cheat, 20);
    }

    size_t solve(const std::vector<std::string>& rows, int min_cheat, int cheat_length) {
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

        std::set<std::pair<advent::coord, advent::coord>> cheats;
        for (const auto& [pos, timing] : timings) {
            for (int delta_row = -cheat_length; delta_row <= cheat_length; delta_row++) {
                auto remaining = cheat_length - std::abs(delta_row);
                for (int delta_column = -remaining; delta_column <= remaining; delta_column++) {
                    auto manhattan = std::abs(delta_row) + std::abs(delta_column);
                    auto shortcut = pos + delta_row * advent::direction::down() + delta_column * advent::direction::right();
                    if (timings.count(shortcut) && timings[shortcut] - timing - manhattan >= min_cheat) {
                        cheats.insert(std::make_pair(pos, shortcut));
                    }
                }
            }
        }

        return cheats.size();
    }
}
