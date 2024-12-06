#pragma once

#include <set>
#include <string>
#include <utility>
#include <vector>
#include <iostream>

namespace day06 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    struct coord {
        int row;
        int column;

        coord turn_right() { return {column, -row}; }

        friend bool operator==(const coord& l, const coord& r) {
            return l.row == r.row && l.column == r.column;
        }
        friend bool operator<(const coord& l, const coord& r) {
            return l.row == r.row ? l.column < r.column : l.row < r.row;
        }
        friend coord operator+(const coord& l, const coord& r) {
            return {l.row + r.row, l.column + r.column};
        }
    };

    class lab {
    public:
        lab(const coord& size, coord& guard, std::set<coord>& obstacles);
        bool guard_left() const;
        void move_guard();
        int visited() const;
        const coord& position_of_guard() const;
        void add_obstacle(const coord& where);
        bool guard_is_looping() const;
        int height() const;
        int width() const;
        void set_part2();
    private:
        const coord size_;
        coord guard_;
        coord direction_;
        std::set<coord> obstacles_;
        std::set<coord> seen_;
        std::set<std::pair<coord, coord>> seen_and_same_direction_;
        bool loop_;
        bool part2_;
    };

    lab parse(const std::vector<std::string>& rows);
}
