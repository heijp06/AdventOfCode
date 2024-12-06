#pragma once

#include <set>
#include <string>
#include <vector>
#include <iostream>

namespace day06 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    struct coord {
        int row;
        int column;
        friend bool operator<(const coord& l, const coord& r) {
            return l.row == r.row ? l.column < r.column : l.row < r.row;
        }
    };

    class lab {
    public:
        lab(const coord& size, coord& guard, const std::set<coord>& obstacles);
    private:
        const coord size_;
        coord guard_;
        coord direction_;
        const std::set<coord> obstacles_;
    };

    lab parse(const std::vector<std::string>& rows);
}
