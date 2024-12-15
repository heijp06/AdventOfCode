#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        auto it = empty_row(rows);
        auto& grid = advent::grid(std::vector<std::string>(rows.cbegin(), it));
        const auto& directions = parse_directions(std::vector<std::string>(it + 1, rows.cend()));
        auto& robot = grid.find('@');

        for (const auto& direction : directions) {
            advent::coord box = robot + direction;
            while (grid[box] == 'O') {
                box += direction;
            }

            if (grid[box] == '#') {
                continue;
            }

            grid[box] = 'O';
            grid[robot] = '.';
            robot += direction;
            grid[robot] = '@';
        }

        auto result{0};

        for (const auto& position : grid.find_all('O')) {
            result += 100 * position.row + position.column;
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<std::string>::const_iterator empty_row(const std::vector<std::string>& rows) {
        auto it = rows.cbegin();

        while (!it->empty()) {
            ++it;
        }

        return it;
    }

    std::vector<advent::direction> parse_directions(const std::vector<std::string>& rows) {
        std::vector<advent::direction> directions;

        for (const auto& row : rows) {
            for (const auto& c : row) {
                switch (c) {
                case '^':
                    directions.push_back(advent::direction::up());
                    break;
                case 'v':
                    directions.push_back(advent::direction::down());
                    break;
                case '<':
                    directions.push_back(advent::direction::left());
                    break;
                case '>':
                    directions.push_back(advent::direction::right());
                    break;
                }
            }
        }

        return directions;
    }
}
