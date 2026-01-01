#include <algorithm>
#include <iostream>

#include "day10.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows) {
        auto lights = std::vector<Light>{};
        lights.reserve(rows.size());
        int seconds{};

        for (const auto& row : rows) {
            const auto& fields = advent::ints(row);
            lights.push_back({{fields[1], fields[0]}, {fields[3], fields[2]}});
        }

        int previous_height{};
        int height{-1};
        int min_row{};
        int max_row{};
        do {
            seconds++;
            previous_height = height;
            for (auto& light : lights) {
                light.position = light.position + light.velocity;
            }

            min_row = std::min_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
                return left.position.row < right.position.row;
                })->position.row;
            max_row = std::max_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
                return left.position.row < right.position.row;
                })->position.row;
            height = max_row - min_row + 1;
        } while (previous_height == -1 || height < previous_height);

        for (auto& light : lights) {
            light.position = light.position - light.velocity;
        }

        min_row = std::min_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
            return left.position.row < right.position.row;
            })->position.row;
        max_row = std::max_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
            return left.position.row < right.position.row;
            })->position.row;
        const auto& min_column = std::min_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
            return left.position.column < right.position.column;
            })->position.column;
        const auto& max_column = std::max_element(lights.cbegin(), lights.cend(), [](const auto& left, const auto& right) {
            return left.position.column < right.position.column;
            })->position.column;

        auto grid = advent::grid(max_row - min_row + 1, max_column - min_column + 1);

        for (auto& light : lights) {
            grid[{light.position.row - min_row, light.position.column - min_column}] = '#';
        }

        std::cout << std::endl;
        grid.draw();

        return seconds - 1;
    }

    int part2(const std::vector<std::string>& rows) {
        return part1(rows);
    }
}
