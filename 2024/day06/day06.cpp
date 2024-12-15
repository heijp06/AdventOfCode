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
        const auto grid = advent::grid(rows);
        coord guard;
        std::set<coord> obstacles;

        for (int r = 0; r < grid.get_height(); r++) {
            for (int c = 0; c < grid.get_width(); c++) {
                switch (grid[{r, c}]) {
                case '^':
                    guard = {r, c};
                    break;
                case '#':
                    obstacles.insert({r, c});
                    break;
                }
            }
        }

        return lab(grid, guard, obstacles);
    }

    lab::lab(const advent::grid& grid, coord& guard, std::set<coord>& obstacles) :
        grid_{grid},
        guard_{guard},
        direction_{-1, 0},
        obstacles_{obstacles},
        seen_{},
        seen_and_same_direction_{},
        loop_{false},
        part2_{false} {
    }

    bool lab::guard_left() const {
        return guard_.row < 0 || guard_.row >= grid_.get_height()
            || guard_.column < 0 || guard_.column >= grid_.get_width();
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
        return static_cast<int>(seen_.size());
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
        return grid_.get_height();
    }

    int lab::width() const {
        return grid_.get_width();
    }

    void lab::set_part2() {
        part2_ = true;
    }

    std::set<coord> lab::get_seen() const {
        return seen_;
    }
}
