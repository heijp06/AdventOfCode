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

        for (auto& region : regions) {
            if (region.edge.empty()) {
                continue;
            }

            for (const auto& pos : region.edge) {
                grid[pos] = 'X';
            }
        }

        for (int row = min_row; row <= max_row; row++) {
            for (int column = min_column; column <= max_column; column++) {
                auto min_distance = max_row + max_column + 10;
                auto count = 0;
                for (const auto& region : regions) {
                    const auto& pos = region.area.front();
                    auto distance = std::abs(column - pos.column) + std::abs(row - pos.row);
                    if (distance < min_distance) {
                        count = 1;
                        min_distance = distance;
                    }
                    else if (distance == min_distance) {
                        count++;
                    }
                }
                if (count > 1) {
                    grid[{row, column}] = 'X';
                }
            }
        }

        //grid.draw();
        //std::cout << std::endl;

        bool growing{true};
        std::vector<advent::coord> new_edge{};
        while (growing) {
            growing = false;

            for (auto& region : regions) {
                if (region.edge.empty()) {
                    continue;
                }

                for (const auto& coord : region.edge) {
                    for (const auto& direction : advent::direction::nsew()) {
                        const auto pos = coord + direction;
                        if (pos.column < min_column || pos.column > max_column || pos.row < min_row || pos.row > max_row) {
                            region.infinite = true;
                            continue;
                        }

                        if (grid[pos] == 'X') {
                            continue;
                        }

                        grid[pos] = 'X';
                        new_edge.push_back(pos);
                        region.area.push_back(pos);
                        growing = true;
                    }
                }

                std::swap(region.edge, new_edge);
                new_edge.clear();
            }

            //grid.draw();
            //std::cout << std::endl;
        }

        const auto& largest = std::max_element(
            regions.cbegin(), regions.cend(), [](const auto& left, const auto& right) {
                if (left.infinite == right.infinite) {
                    return left.area.size() < right.area.size();
                }

                if (left.infinite) {
                    return true;
                }

                return false;
            });

        return static_cast<int>(largest->area.size());
    }

    int part2(const std::vector<std::string>& rows, int max) {
        int min_row{-1};
        int max_row{-1};
        int min_column{-1};
        int max_column{-1};

        auto regions = parse(rows, min_column, max_column, min_row, max_row);

        std::vector<int> sum_row{};
        sum_row.reserve(max_row + 1);
        std::vector<int> sum_column{};
        sum_column.reserve(max_column + 1);

        for (int row = 0; row <= max_row; row++) {
            int distance{};
            for (const auto& region : regions) {
                distance += std::abs(row - region.area.front().row);
            }
            sum_row.push_back(distance);
        }

        for (int column = 0; column <= max_column; column++) {
            int distance{};
            for (const auto& region : regions) {
                distance += std::abs(column - region.area.front().column);
            }
            sum_column.push_back(distance);
        }

        int count{};

        for (int row = min_row; row <= max_row; row++) {
            for (int column = min_column; column <= max_column; column++) {
                if (sum_row[row] + sum_column[column] < max) {
                    count++;
                }
            }
        }

        return count;
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
