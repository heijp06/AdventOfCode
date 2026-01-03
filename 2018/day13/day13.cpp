#include <iostream>
#include <string>

#include "day13.h"

namespace day13 {
    std::string part1(const std::vector<std::string>& rows) {
        return solve(rows, false);
    }

    std::string part2(const std::vector<std::string>& rows) {
        return solve(rows, true);
    }

    const std::string solve(const std::vector<std::string>& rows, bool part2) {
        advent::grid grid{rows};
        auto current = parse_carts(grid);
        std::map<advent::coord, Cart> next{};

        while (current.size() > 1) {
            std::vector<std::pair<advent::coord, Cart>> carts(current.cbegin(), current.cend());
            //draw(grid, current);
            for (const auto& cart : carts) {
                if (!current.count(cart.first)) {
                    continue;
                }

                current.erase(cart.first);
                const auto& moved = cart.second.move(grid);
                if (current.count(moved.position) || next.count(moved.position)) {
                    if (part2) {
                        current.erase(moved.position);
                        next.erase(moved.position);
                    }
                    else {
                        return std::to_string(moved.position.column) + ',' + std::to_string(moved.position.row);
                    }
                }
                else {
                    next[moved.position] = moved;
                }
            }
            std::swap(current, next);
        }

        const auto& last = *current.cbegin();
        return std::to_string(last.first.column) + ',' + std::to_string(last.first.row);
    }

    std::map<advent::coord, Cart> parse_carts(advent::grid& grid) {
        std::map<advent::coord, Cart> carts{};
        std::vector<std::pair<std::string, advent::direction>> directions{
            {">-", advent::direction::right()},
            {"v|", advent::direction::down()},
            {"<-", advent::direction::left()},
            {"^|", advent::direction::up()}
        };

        for (const auto& direction : directions) {
            for (const auto& position : grid.find_all(direction.first[0])) {
                carts[position] = {position, direction.second, day13::Left};
                grid[position] = direction.first[1];
            }
        }

        return carts;
    }

    void draw(advent::grid grid, const std::map<advent::coord, Cart>& carts) {
        for (const auto& cart : carts) {
            char c{};
            if (cart.second.direction == advent::direction::right()) c = '>';
            else if (cart.second.direction == advent::direction::down()) c = 'v';
            else if (cart.second.direction == advent::direction::left()) c = '<';
            else if (cart.second.direction == advent::direction::up()) c = '^';
            grid[cart.second.position] = c;
        }
        grid.draw();
        std::cout << std::endl;
    }

    Cart Cart::move(const advent::grid& grid) const {
        const auto& new_position = position + direction;
        switch (grid[new_position]) {
        case '-':
        case '|':
            return {new_position, direction, turn};
        case '/':
            return {new_position, direction.delta_row == 0 ? direction.turn_left() : direction.turn_right(), turn};
        case '\\':
            return {new_position, direction.delta_row == 0 ? direction.turn_right() : direction.turn_left(), turn};
        case '+':
            switch (turn) {
            case day13::Left:
                return {new_position, direction.turn_left(), day13::Straight};
            case day13::Straight:
                return {new_position, direction, day13::Right};
            case day13::Right:
                return {new_position, direction.turn_right(), day13::Left};
            }
        default:
            return *this;
        }
    }
}
