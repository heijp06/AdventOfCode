#include "day06.h"

namespace day06 {
    int part1(const std::vector<std::string>& rows) {
        auto lab = parse(rows);

        while (!lab.guard_left()) {
            lab.move_guard();
        }

        return lab.visited();
    }

    int part2(const std::vector<std::string>& rows) {
        auto lab0 = parse(rows);

        while (!lab0.guard_left()) {
            lab0.move_guard();
        }

        const auto& seen = lab0.get_seen();

        const auto& lab = parse(rows);
        auto result{0};

        for (const auto& c : seen) {
            if (c == lab.position_of_guard()) {
                continue;
            }

            auto new_lab = lab;
            new_lab.set_part2();
            new_lab.add_obstacle(c);

            while (!new_lab.guard_is_looping() && !new_lab.guard_left()) {
                new_lab.move_guard();
            }

            result += new_lab.guard_is_looping();
        }

        return result;
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

    lab::lab(const coord& size, coord& guard, std::set<coord>& obstacles) :
        size_{size},
        guard_{guard},
        direction_{-1, 0},
        obstacles_{obstacles},
        seen_{},
        seen_and_same_direction_{},
        loop_{false},
        part2_{false} {
    }

    bool lab::guard_left() const {
        return guard_.row < 0 || guard_.row >= size_.row
            || guard_.column < 0 || guard_.column >= size_.column;
    }

    void lab::move_guard() {
        if (!part2_) {
            seen_.insert(guard_);
        }
        auto new_position = guard_ + direction_;
        auto direction_changed{false};
        while (obstacles_.count(new_position)) {
            direction_ = direction_.turn_right();
            new_position = guard_ + direction_;
            direction_changed = true;
        }
        if (direction_changed) {
            loop_ = !seen_and_same_direction_.insert(std::make_pair(guard_, direction_)).second;
        }
        guard_ = new_position;
    }

    int lab::visited() const {
        return seen_.size();
    }

    const coord& lab::position_of_guard() const {
        return guard_;
    }

    void lab::add_obstacle(const coord& where) {
        obstacles_.insert(where);
    }

    bool lab::guard_is_looping() const {
        return loop_;
    }

    int lab::height() const {
        return size_.row;
    }

    int lab::width() const {
        return size_.column;
    }

    void lab::set_part2() {
        part2_ = true;
    }

    std::set<coord> lab::get_seen() const {
        return seen_;
    }
}
