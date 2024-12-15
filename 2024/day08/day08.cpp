#include <set>

#include "day08.h"

namespace day08 {
    size_t part1(const std::vector<std::string>& rows) {
        const auto& grid = parse(rows);
        std::set<advent::coord> antinodes;

        for (auto const& [_, frequencies] : grid.get_antennas()) {
            for (size_t i = 0; i < frequencies.size() - 1; i++) {
                const auto& frequency1 = frequencies[i];
                for (size_t j = i + 1; j < frequencies.size(); j++) {
                    const auto& frequency2 = frequencies[j];
                    const auto& delta = frequency2 - frequency1;
                    const auto& antinode1 = frequency1 - delta;
                    if (antinode1.row >= 0 && antinode1.row < grid.get_height() && antinode1.column >= 0 && antinode1.column < grid.get_width()) {
                        antinodes.insert(antinode1);
                    }
                    const auto& antinode2 = frequency2 + delta;
                    if (antinode2.row >= 0 && antinode2.row < grid.get_height() && antinode2.column >= 0 && antinode2.column < grid.get_width()) {
                        antinodes.insert(antinode2);
                    }
                }

            }
        }

        return antinodes.size();
    }

    size_t part2(const std::vector<std::string>& rows) {
        const auto& grid = parse(rows);
        std::set<advent::coord> antinodes;

        for (auto const& [_, frequencies] : grid.get_antennas()) {
            for (size_t i = 0; i < frequencies.size() - 1; i++) {
                const auto& frequency1 = frequencies[i];
                for (size_t j = i + 1; j < frequencies.size(); j++) {
                    const auto& frequency2 = frequencies[j];
                    const auto& delta = frequency2 - frequency1;
                    auto antinode = frequency1;
                    while (antinode.row >= 0 && antinode.row < grid.get_height() && antinode.column >= 0 && antinode.column < grid.get_width()) {
                        antinodes.insert(antinode);
                        antinode = antinode + delta;
                    }
                    antinode = frequency1;
                    while (antinode.row >= 0 && antinode.row < grid.get_height() && antinode.column >= 0 && antinode.column < grid.get_width()) {
                        antinodes.insert(antinode);
                        antinode = antinode - delta;
                    }
                }

            }
        }

        return antinodes.size();
    }

    const grid parse(std::vector<std::string> rows) {
        const auto g = advent::grid(rows);
        std::map<char, std::vector<advent::coord>> antennas;

        for (int row = 0; row < g.get_height(); row++) {
            for (int column = 0; column < g.get_width(); column++) {
                auto antenna = g[{row, column}];
                if (antenna != '.') {
                    antennas[antenna].push_back({row, column});
                }
            }
        }

        return grid({g.get_height(), g.get_width()}, antennas);
    }

    grid::grid(const advent::coord& size, const antennas_t& antennas) :
        size_{size},
        antennas_{antennas} {
    }

    int grid::get_height() const {
        return size_.row;
    }

    int grid::get_width() const {
        return size_.column;
    }

    const antennas_t& grid::get_antennas() const {
        return antennas_;
    }
}
