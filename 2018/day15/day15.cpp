#include "day15.h"
#include <iostream>

#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        return solve(rows);
    }

    int solve(const std::vector<std::string>& rows, int elves_power) {
        advent::grid grid{rows};
        auto units = get_units(grid, elves_power);
        std::vector<advent::coord> positions;
        positions.reserve(units.size());
        int round{};

        //grid.draw();
        //dump_hp(units);

        while (true) {
            for (const auto& pair : units) {
                positions.push_back(pair.first);
            }

            for (const auto& position : positions) {
                if (!units.count(position)) {
                    continue;
                }

                auto unit = units[position];

                if (!has_targets(units, unit)) {
                    return score(units, round);
                }

                auto& step = find_step(grid, unit);
                move(grid, units, unit, step);

                attack(grid, units, unit);

                //grid.draw();
                //dump_hp(units);
            }

            //grid.draw();
            //dump_hp(units);

            positions.clear();
            round++;
        }

        return -1;
    }

    int score(std::map<advent::coord, std::shared_ptr<Unit>>& units, int round) {
        int score{};

        for (const auto& pair : units) {
            score += pair.second->get_hit_points();
        }
        score *= round;

        return score;
    }

    advent::coord find_step(const advent::grid& grid, const std::shared_ptr<Unit> unit) {
        auto enemy = unit->is_elve() ? 'G' : 'E';
        for (const auto& direction : advent::direction::nwes()) {
            if (grid[unit->get_position() + direction] == enemy) {
                return unit->get_position();
            }
        }

        advent::grid copy = grid;
        copy[unit->get_position()] = 'X';
        std::vector<std::pair<advent::coord, advent::coord>> current{};
        current.reserve(100);
        std::vector<std::pair<advent::coord, advent::coord>> next{};
        next.reserve(100);

        for (const auto& direction : advent::direction::nwes()) {
            const auto& new_position = unit->get_position() + direction;
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

        return unit->get_position();
    }

    void move(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit, const advent::coord& step) {
        if (unit->get_position() == step) {
            return;
        }

        grid[unit->get_position()] = '.';
        grid[step] = unit->is_elve() ? 'E' : 'G';
        units.erase(unit->get_position());
        unit->set_position(step);
        units.insert({unit->get_position(), unit});
    }

    void dump_hp(std::map<advent::coord, std::shared_ptr<Unit>>& units) {
        for (const auto& pair : units) {
            std::cout << pair.first.row << ',' << pair.first.column << ' ' << pair.second->get_hit_points() << "   ";
        }
        std::cout << std::endl;
    }

    void attack(advent::grid& grid, std::map<advent::coord, std::shared_ptr<Unit>>& units, std::shared_ptr<Unit> unit) {
        auto enemy_symbol = unit->is_elve() ? 'G' : 'E';
        std::shared_ptr<Unit> enemy = nullptr;
        int hp{};

        for (const auto& direction : advent::direction::nwes()) {
            advent::coord& new_position = unit->get_position() + direction;
            if (grid[new_position] == enemy_symbol && (hp == 0 || units[new_position]->get_hit_points() < hp)) {
                hp = units[new_position]->get_hit_points();
                enemy = units[new_position];
            }
        }

        if (enemy) {
            enemy->damage(unit->get_attack_power());
            if (enemy->get_hit_points() <= 0) {
                grid[enemy->get_position()] = '.';
                units.erase(enemy->get_position());
            }
        }
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::map<advent::coord, std::shared_ptr<Unit>> get_units(const advent::grid& grid, int elves_power) {
        std::map<advent::coord, std::shared_ptr<Unit>> units{};

        for (const auto& pair : grid.find_all("GE")) {
            auto is_elve = pair.second == 'E';
            auto power = is_elve ? elves_power : 3;
            units.insert({pair.first, std::make_shared<Unit>(pair.first, pair.second == 'E', elves_power)});
        }

        return units;
    }

    bool has_targets(const std::map<advent::coord, std::shared_ptr<Unit>>& units, const std::shared_ptr<Unit> unit) {
        for (const auto& pair : units) {
            if (unit->is_elve() != pair.second->is_elve()) {
                return true;
            }
        }

        return false;
    }

    Unit::Unit(advent::coord position, bool is_elve, int attack_power)
        : position_{position}, is_elve_{is_elve}, hit_points_{200}, attack_power_{attack_power} {
    }

    const advent::coord Unit::get_position() const {
        return position_;
    }

    void Unit::set_position(const advent::coord& position) {
        position_ = position;
    }

    const bool Unit::is_elve() const {
        return is_elve_;
    }

    const int Unit::get_hit_points() const {
        return hit_points_;
    }

    void Unit::damage(int attack_power) {
        hit_points_ -= attack_power;
    }

    int Unit::get_attack_power() const {
        return attack_power_;
    }
}
