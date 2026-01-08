#include <iostream>

#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        advent::grid grid{rows};
        auto units = get_units(grid);
        std::vector<advent::coord> positions;
        positions.reserve(units.size());
        int round{};

        grid.draw();

        while (true) {
            for (const auto& pair : units) {
                positions.push_back(pair.first);
            }

            for (const auto& position : positions) {
                if (!units.count(position)) {
                    continue;
                }

                auto& unit = units[position];

                if (!has_targets(units, unit)) {
                    return score(units, round);
                }

                auto& step = find_step(grid, unit);
                move(grid, unit, step);
            }

            break;
        }

        grid.draw();
        std::cout << std::endl;

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
        auto enemy = unit.is_elve() ? 'G' : 'E';
        for (const auto& direction : advent::direction::nwes()) {
            if (grid[unit.get_position() + direction] == enemy) {
                return unit.get_position();
            }
        }

        advent::grid copy = grid;
        copy[unit.get_position()] = 'X';
        std::vector<std::pair<advent::coord, advent::coord>> current{};
        current.reserve(100);
        std::vector<std::pair<advent::coord, advent::coord>> next{};
        next.reserve(100);

        for (const auto& direction : advent::direction::nwes()) {
            const auto& new_position = unit.get_position() + direction;
            if (copy[new_position] == '.') {
                current.push_back({new_position, new_position});
            }
        }

        while (!current.empty()) {
            for (const auto& pair : current) {
                advent::coord start{};
                advent::coord end{};
                bool found{};
                for (const auto& direction : advent::direction::nwes()) {
                    const auto& new_position = pair.second + direction;
                    if (copy[new_position] == enemy) {
                        if (!found || pair.second < end) {
                            start = pair.first;
                            end = pair.second;
                            found = true;
                        }
                    }
                    else if (copy[new_position] == '.') {
                        next.push_back({pair.first, new_position});
                        copy[new_position] = 'X';
                    }
                }
                if (found) {
                    return start;
                }
            }

            std::swap(current, next);
            next.clear();
        }

        return unit.get_position();
    }

    void move(advent::grid& grid, Unit& unit, const advent::coord& step) {
        if (unit.get_position() == step) {
            return;
        }

        grid[unit.get_position()] = '.';
        grid[step] = unit.is_elve() ? 'E' : 'G';
        unit.set_position(step);
    }

    void attack(advent::grid& grid, std::map<advent::coord, Unit>& units, Unit& unit) {
        //auto enemy = unit.is_elve() ? 'G' : 'E';
        //for (const auto& direction : advent::direction::nwes()) {
        //    advent::direction direction{};
        //    int hp{};
        //    advent::coord& new_position = unit.get_position() + direction;
        //    if (grid[new_position] == enemy && (hp == 0 || units[new_position].get_hit_points() < hp)) {
        //        hp = units[new_position].get_hit_points();
        //        direction = 
        //    }
        //}
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

    void Unit::set_position(const advent::coord& position) {
        position_ = position;
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
