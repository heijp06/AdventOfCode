#include <cmath>
#include <map>
#include <set>
#include <utility>

#include "day20.h"
#include "../../lib/advent.h"

namespace day20 {
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
                if (timings.count(shortcut) && timings[shortcut] - timing - 2 >= min_cheat) {
                    result++;
                }
            }
        }

        return result;
    }

    size_t part2(const std::vector<std::string>& rows, int min_cheat) {
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
            for (int delta_row = -20; delta_row <= 20; delta_row++) {
                for (int delta_column = -20; delta_column <= 20; delta_column++) {
                    auto manhattan = std::abs(delta_row) + std::abs(delta_column);
                    if (manhattan > 20) {
                        continue;
                    }
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
