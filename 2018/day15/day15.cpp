#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        advent::grid grid{rows};
        auto current = get_units(grid);

        return -1;
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
