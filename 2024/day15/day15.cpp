#include <map>

#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        auto it = empty_row(rows);
        auto& grid = advent::grid(std::vector<std::string>(rows.cbegin(), it));
        const auto& directions = parse_directions(std::vector<std::string>(it + 1, rows.cend()));
        auto& robot = grid.find('@');

        for (const auto& direction : directions) {
            move(grid, robot, direction);
        }

        return sum_of_gps_coordinates(grid, 'O');
    }

    int part2(const std::vector<std::string>& rows) {
        auto it = empty_row(rows);
        auto& grid = widen(advent::grid(std::vector<std::string>(rows.cbegin(), it)));
        const auto& directions = parse_directions(std::vector<std::string>(it + 1, rows.cend()));
        auto& robot = grid.find('@');

        for (const auto& direction : directions) {
            if (direction.delta_row == 0) {
                move(grid, robot, direction, false);
                continue;
            }

            std::set<advent::coord> to_move{};
            std::set<advent::coord> active{robot};
            auto hit_wall{false};
            while (!active.empty() && !hit_wall) {
                std::set<advent::coord> new_active;
                for (const auto& position : active) {
                    auto new_position1 = position + direction;
                    char c = grid[new_position1];

                    if (c == '#') {
                        hit_wall = true;
                        break;
                    }

                    if (c == '.') {
                        continue;
                    }

                    auto new_position2 = advent::coord{new_position1.row, new_position1.column + (c == '[' ? 1 : -1)};
                    new_active.insert(new_position1);
                    new_active.insert(new_position2);
                }
                to_move.insert(new_active.cbegin(), new_active.cend());
                active = new_active;
            }

            if (hit_wall) {
                continue;
            }

            std::map<advent::coord, char> items;
            for (const auto& position : to_move) {
                items[position] = grid[position];
                grid[position] = '.';
            }

            for (const auto& position : to_move) {
                grid[{position.row + direction.delta_row, position.column}] = items.count(position) ? items[position] : '.';
            }

            grid[robot] = '.';
            robot += direction;
            grid[robot] = '@';
        }

        return sum_of_gps_coordinates(grid, '[');
    }

    int sum_of_gps_coordinates(advent::grid& grid, char c) {
        auto result{0};

        for (const auto& position : grid.find_all(c)) {
            result += 100 * position.row + position.column;
        }

        return result;
    }

    advent::grid widen(const advent::grid& grid) {
        std::vector<std::string> widened;

        for (int row = 0; row < grid.get_height(); row++) {
            std::string line;
            for (int column = 0; column < grid.get_width(); column++) {
                switch (grid[{row, column}]) {
                case '#':
                    line += "##";
                    break;
                case '.':
                    line += "..";
                    break;
                case '@':
                    line += "@.";
                    break;
                case 'O':
                    line += "[]";
                    break;
                }
            }
            widened.push_back(line);
        }

        return widened;
    }

    void day15::move(advent::grid& grid, advent::coord& robot, const advent::direction& direction, bool part1) {
        auto& box = robot + direction;
        while (grid[box] == 'O' || grid[box] == '[' || grid[box] == ']') {
            box += direction;
        }

        if (grid[box] != '#') {
            if (part1) {
                grid[box] = 'O';
            }
            else {
                char open_char = direction == advent::direction::right() ? '[' : ']';
                char close_char = direction == advent::direction::right() ? ']' : '[';
                auto open{true};
                for (auto position = robot + direction + direction; position != box + direction; position += direction) {
                    grid[position] = open ? open_char : close_char;
                    open = !open;
                }
            }

            grid[robot] = '.';
            robot += direction;
            grid[robot] = '@';
        }
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
