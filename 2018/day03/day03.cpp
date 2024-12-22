#include <cmath>

#include "day03.h"
#include "../../lib/advent.h"

namespace day03 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<claim> claims;

        auto height{0};
        auto width{0};
        for (const auto& row : rows) {
            const auto& claim = parse(row);
            height = std::max(height, claim.top + claim.height);
            width = std::max(width, claim.left + claim.width);
            claims.push_back(claim);
        }

        std::vector<std::vector<int>> grid;
        for (int row = 0; row < height; row++) {
            grid.push_back(std::vector<int>(width));
        }

        for (const auto& claim : claims) {
            for (int row = 0; row < claim.height; row++) {
                auto& line = grid[claim.top + row];
                for (int column = 0; column < claim.width; column++) {
                    line[claim.left + column]++;
                }
            }
        }

        auto result{0};
        for (const auto& line : grid) {
            for (const auto& square: line) {
                result += square >= 2;
            }
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        std::vector<claim> claims;

        auto height{0};
        auto width{0};
        for (const auto& row : rows) {
            const auto& claim = parse(row);
            height = std::max(height, claim.top + claim.height);
            width = std::max(width, claim.left + claim.width);
            claims.push_back(claim);
        }

        std::vector<std::vector<int>> grid;
        for (int row = 0; row < height; row++) {
            grid.push_back(std::vector<int>(width));
        }

        for (const auto& claim : claims) {
            for (int row = 0; row < claim.height; row++) {
                auto& line = grid[claim.top + row];
                for (int column = 0; column < claim.width; column++) {
                    line[claim.left + column]++;
                }
            }
        }

        for (const auto& claim : claims) {
            auto valid{true};
            for (int row = 0; row < claim.height && valid; row++) {
                auto& line = grid[claim.top + row];
                for (int column = 0; column < claim.width && valid; column++) {
                    if (line[claim.left + column] != 1) {
                        valid = false;
                    }
                }
            }
            if (valid) {
                return claim.id;
            }
        }

        return -1;
    }

    claim parse(std::string row) {
        const auto& ints = advent::ints(row);
        return claim{ints[0], ints[1], ints[2], ints[3], ints[4]};
    }
}
