#include <map>

#include "day10.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows)
    {
        return solve(rows);
    }

    int part2(const std::vector<std::string>& rows) {
        return solve(rows, true);
    }

    int solve(const std::vector<std::string>& rows, bool part2)
    {
        auto result{0};
        int height = rows.size();
        int width = rows[0].size();

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
                if (line[column] == '0') {
                    result += count(rows, height, width, { row, column }, part2);
                }
            }
        }

        return result;
    }

    int count(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord, bool part2) {
        std::map<advent::coord, int> positions{{coord, 1}};
        std::vector<advent::coord> directions{ {1, 0}, {0, 1}, {-1, 0}, {0, -1} };

        for (int i = 0; i < 9; i++) {
            std::map<advent::coord, int> new_positions;
            char c = '1' + i;
            for (const auto& [position, count] : positions) {
                for (const auto& direction: directions) {
                    const auto& new_position = position + direction;
                    if (new_position.row >= 0 && new_position.row < height &&
                        new_position.column >= 0 && new_position.column < width &&
                        rows[new_position.row][new_position.column] == c) {
                        new_positions[new_position] += count;
                    }
                }
            }

            positions = new_positions;
        }

        if (!part2) {
            return positions.size();
        }

        int result{};

        for (const auto& [_, count]: positions) {
            result += count;
        }

        return result;
    }
}
