#include "day12.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows) {
        return solve(rows, &perimeter);
    }

    int part2(const std::vector<std::string>& rows) {
        return solve(rows, &sides);
    }

    int solve(const std::vector<std::string>& rows, std::function<int(std::set<advent::coord>)> cost) {
        int height = rows.size();
        int width = rows[0].size();
        std::set<advent::coord> seen;
        std::vector<std::set<advent::coord>> regions;

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
                auto position = advent::coord{row, column};
                if (seen.count(position)) {
                    continue;
                }

                const auto& region = create_region(rows, position);
                regions.push_back(region);
                seen.insert(region.cbegin(), region.cend());
            }
        }

        auto result{0};
        for (const auto& region : regions) {
            result += cost(region) * static_cast<int>(region.size());
        }

        return result;
    }

    std::set<advent::coord> create_region(const std::vector<std::string>& rows, const advent::coord& position) {
        int height = rows.size();
        int width = rows[0].size();
        auto plant = rows[position.row][position.column];
        std::set region{position};
        std::set active{position};
        std::vector<advent::coord> directions = { {1, 0}, {-1, 0}, {0, 1}, {0,-1} };

        while (!active.empty()) {
            std::set<advent::coord> new_active;
            for (const auto& active_position:active) {
                for (const auto& direction : directions) {
                    const auto& new_position = active_position + direction;
                    if (new_position.row < 0 || new_position.row >= height
                        || new_position.column < 0 || new_position.column >= width) {
                        continue;
                    }

                    if (region.count(new_position) || rows[new_position.row][new_position.column] != plant) {
                        continue;
                    }

                    region.insert(new_position);
                    new_active.insert(new_position);
                }
            }
            active = new_active;
        }

        return region;
    }

    int perimeter(const std::set<advent::coord>& region)
    {
        auto result{0};
        std::vector<advent::coord> directions = { {1, 0}, {-1, 0}, {0, 1}, {0,-1} };

        for (const auto& position : region) {
            for (const auto& direction : directions) {
                result += !region.count(position + direction);
            }
        }

        return result;
    }

    int sides(const std::set<advent::coord>& region) {
        auto fences{0};
        std::set<edge> seen;
        std::vector<advent::coord> directions = { {1, 0}, {-1, 0}, {0, 1}, {0,-1} };

        for (const auto& position : region) {
            for (const auto& outside : directions) {
                if (region.count(position + outside)) {
                    continue;
                }

                edge current{position, outside};
                advent::coord direction = {outside.column, -outside.row};
                while (!seen.count(current)) { 
                    seen.insert(current);

                    if (region.count(current.position + direction)) {
                        if (region.count(current.position + direction + current.outside)) {
                            fences++;
                            current = {
                                current.position + direction + current.outside,
                                {-current.outside.column, current.outside.row}};
                            direction = {-direction.column, direction.row};
                        }
                        else {
                            current = {current.position + direction, current.outside};
                        }
                    }
                    else {
                        fences++;
                        current = {current.position, {current.outside.column, -current.outside.row}};
                        direction = {direction.column, -direction.row};
                    }
                }
            }
        }

        return fences;
    }
}
