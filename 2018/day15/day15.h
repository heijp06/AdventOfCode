#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day15 {
    class Unit {
    public:
        Unit();
        Unit(advent::coord position, bool is_elve);
        const advent::coord get_position() const;
        void set_position(const advent::coord& position);
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

    std::map<advent::coord, std::shared_ptr<Unit>> get_units(const advent::grid& grid);
    bool has_targets(const std::map<advent::coord, std::shared_ptr<Unit>>& units, const std::shared_ptr<Unit> unit);
    int score(std::map<advent::coord, std::shared_ptr<Unit>>& units, int round);
    advent::coord find_step(const advent::grid& grid, const std::shared_ptr<Unit> unit);
    void move(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit, const advent::coord& step);
    void attack(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit);
}
