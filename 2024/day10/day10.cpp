#include <map>
#include <set>

#include "day10.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows) {
        auto result{0};
        int height = rows.size();
        int width = rows[0].size();

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
				if (line[column] == '0') {
                    result += score(rows, height, width, { row, column });
				}
            }
        }


        return result;
    }

    int64_t part2(const std::vector<std::string>& rows) {
        int64_t result{};
        int height = rows.size();
        int width = rows[0].size();

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
				if (line[column] == '0') {
                    result += rating(rows, height, width, { row, column });
				}
            }
        }


        return result;
    }

    int score(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord)
    {
        std::set<advent::coord> positions{coord};
        std::vector<advent::coord> directions{ {1, 0}, {0, 1}, {-1, 0}, {0, -1} };

        for (int i = 0; i < 9; i++) {
            std::set<advent::coord> new_positions;
            char c = '1' + i;
            for (const auto& position: positions) {
				for (const auto& direction: directions) {
                    const auto& new_position = position + direction;
                    if (new_position.row >= 0 && new_position.row < height &&
                        new_position.column >= 0 && new_position.column < width &&
                        rows[new_position.row][new_position.column] == c) {
                        new_positions.insert(new_position);
                    }
				}
            }

            positions = new_positions;
        }

        return positions.size();
    }

    int64_t rating(const std::vector<std::string>& rows, int height, int width, const advent::coord& coord)
    {
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

        int64_t result{};

        for (const auto& [_, count]: positions)
        {
            result += count;
        }

        return result;
    }
}
