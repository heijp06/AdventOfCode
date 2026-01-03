#include "day15.h"
#include "day15.h"
#include "day15.h"
#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        advent::grid grid{rows};
        auto units = get_units(grid);
        std::vector<advent::coord> positions;
        positions.reserve(units.size());
        int round{};

        while (true) {
            for (const auto& pair : units) {
                positions.push_back(pair.first);
            }

            for (const auto& position : positions) {
                auto& unit = units[position];

                if (!has_targets(units, unit)) {
                    return score(units, round);
                }

                // Find square in range
                auto& square = find_step(grid, unit);
                // Move
                // If in range attack
            }

            break;
        }

        return -1;
    }

    int score(std::map<advent::coord, day15::Unit>& units, int round) {
        int score{};

        for (const auto& pair : units) {
            score += pair.second.get_hit_points();
        }
        score *= round;

        return score;
    }

    advent::coord find_step(const advent::grid& grid, const Unit& unit) {
        auto symbol = unit.is_elve() ? 'E' : 'G';

        return advent::coord();
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::map<advent::coord, Unit> get_units(const advent::grid& grid) {
        std::map<advent::coord, Unit> units{};

        for (const auto& pair : grid.find_all("GE")) {
            units.insert({pair.first, Unit{pair.first, pair.second == 'E'}});
        }

        return units;
    }

    bool day15::has_targets(const std::map<advent::coord, Unit>& units, const Unit& unit) {
        for (const auto& pair : units) {
            if (unit.is_elve() != pair.second.is_elve()) {
                return true;
            }
        }

        return false;
    }

    Unit::Unit()
        : position_{0, 0}, is_elve_{false}, hit_points_{} {
    }

    Unit::Unit(advent::coord position, bool is_elve)
        : position_{position}, is_elve_{is_elve}, hit_points_{200} {
    }

    const advent::coord Unit::get_position() const {
        return position_;
    }

    const bool day15::Unit::is_elve() const {
        return is_elve_;
    }

    const int day15::Unit::get_hit_points() const {
        return hit_points_;
    }

    void day15::Unit::damage() {
        hit_points_ -= 3;
    }
}
