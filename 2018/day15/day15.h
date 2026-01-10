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
        Unit(advent::coord position, bool is_elve, int attack_power);
        const advent::coord get_position() const;
        void set_position(const advent::coord& position);
        const bool is_elve() const;
        const int get_hit_points() const;
        void damage(int attack_power);
        int get_attack_power() const;
    private:
        advent::coord position_;
        bool is_elve_;
        int hit_points_;
        int attack_power_;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int solve(const std::vector<std::string>& rows, int elves_power = 3);
    std::map<advent::coord, std::shared_ptr<Unit>> get_units(const advent::grid& grid, int elves_power);
    bool has_targets(const std::map<advent::coord, std::shared_ptr<Unit>>& units, const std::shared_ptr<Unit> unit);
    int score(std::map<advent::coord, std::shared_ptr<Unit>>& units, int round);
    advent::coord find_step(const advent::grid& grid, const std::shared_ptr<Unit> unit);
    void move(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit, const advent::coord& step);
    void attack(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit);
    void dump_hp(std::map<advent::coord, std::shared_ptr<Unit>>& units);
}
