#include "day06.h"

namespace day06 {
    int part1(const std::vector<std::string>& rows) {
        auto& lab = parse(rows);

        while (!lab.guard_left()) {
            lab.move_guard();
        }

        return lab.visited();
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    lab parse(const std::vector<std::string>& rows) {
        int width = static_cast<int>(rows[0].size());
        int height = static_cast<int>(rows.size());

        coord guard;
        std::set<coord> obstacles;

        for (int r = 0; r < height; r++) {
            const auto& row{rows.at(r)};
            for (int c = 0; c < width; c++) {
                switch (row.at(c)) {
                case '^':
                    guard = {r, c};
                    break;
                case '#':
                    obstacles.insert({r, c});
                    break;
                }
            }
        }

        return lab(coord{height, width}, guard, obstacles);
    }

    lab::lab(const coord& size, coord& guard, const std::set<coord>& obstacles) :
        size_{size},
        guard_{guard},
        direction_{0, -1},
        obstacles_{obstacles},
        seen_{{guard}} {
    }

    bool lab::guard_left() const {
        return guard_.row < 0 || guard_.row >= size_.row
            || guard_.column < 0 || guard_.column >= size_.column;
    }

    void lab::move_guard() {
        seen_.insert(guard_);
        auto new_position = guard_ + direction_;
        while (obstacles_.count(new_position)) {
            direction_ = direction_.turn_right();
            new_position = guard_ + direction_;
        }
        guard_ = new_position;
    }

    int lab::visited() const {
        return seen_.size();
    }
}
