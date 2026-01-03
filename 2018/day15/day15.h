#pragma once

#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day15 {
    class Unit {
    public:
        Unit(advent::coord position, bool is_elve);
        const advent::coord get_position() const;
        const bool is_elve() const;
        const int get_hit_points() const;
        void damage();
    private:
        advent::coord position_;
        bool is_elve_;
        int hit_points_;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
