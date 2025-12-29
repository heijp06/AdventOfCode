#include <algorithm>
#include <iostream>

#include "day06.h"

namespace day06 {
    int part1(const std::vector<std::string>& rows) {
        int min_row{-1};
        int max_row{-1};
        int min_column{-1};
        int max_column{-1};

        auto regions = parse(rows, min_column, max_column, min_row, max_row);

        auto grid = advent::grid(max_row + 1, max_column + 1);
        bool growing{true};
        while (growing) {
            growing = false;

            for (auto& region : regions) {
                if (region.edge.empty()) {
                    continue;
                }

                for (const auto& pos : region.edge) {
                    grid[pos] = 'X';
                }
            }

            grid.draw();
            std::cout << std::endl;

            for (auto& region : regions) {
                if (region.edge.empty()) {
                    continue;
                }

                std::set<advent::coord> new_edge{};
                std::set<advent::coord> candidates{};

                for (const auto& coord : region.edge) {
                    for (const auto& direction : advent::direction::nsew()) {
                        candidates.insert(coord + direction);
                    }
                }

                for (const auto& pos : candidates) {
                        if (pos.column < min_column || pos.column > max_column || pos.row < min_row || pos.row > max_row) {
                            region.infinite = true;
                            continue;
                        }

                        if (grid[pos] == 'X' || grid[pos] == ' ') {
                            continue;
                        }

                        if (grid[pos] == '?') {
                            grid[pos] = ' ';
                            continue;
                        }

                        grid[pos] = '?';
                        new_edge.insert(pos);
                }

                std::swap(region.edge, new_edge);
            }

            grid.draw();
            std::cout << std::endl;

            for (auto& region : regions) {
                std::set<advent::coord> erase{};
                for (const auto& coord : region.edge) {
                    if (grid[coord] == ' ') {
                        erase.insert(coord);
                        continue;
                    }

                    growing = true;
                    region.area.insert(coord);
                    grid[coord] = 'X';
                }

                for (const auto& coord : erase) {
                    region.edge.erase(coord);
                }
            }

            grid.draw();
            std::cout << std::endl;
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<Region> parse(const std::vector<std::string>& rows, int& min_column, int& max_column, int& min_row, int& max_row) {
        std::vector<Region> regions{};
        regions.reserve(rows.size());

        for (const auto& row : rows) {
            const auto& fields = advent::ints(row);
            const auto row = fields[1];
            const auto column = fields[0];
            min_column = min_column == -1 ? column : std::min(min_column, column);
            max_column = max_column == -1 ? column : std::max(max_column, column);
            min_row = min_row == -1 ? row : std::min(min_row, row);
            max_row = max_row == -1 ? row : std::max(max_row, row);
            regions.push_back({{{row, column}}, {{row, column}}, false});
        }

        return regions;
    }
}
