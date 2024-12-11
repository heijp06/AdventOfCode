#include <cmath>
#include <stdexcept>

#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    int64_t part1(const std::vector<std::string>& rows) {
        return solve(rows);
    }

    int64_t part2(const std::vector<std::string>& rows) {
        return solve(rows, 75);
    }

    int64_t solve(const std::vector<std::string>& rows, int times) {
        auto stones = parse(rows[0]);

        for (size_t i = 0; i < times; i++) {
            std::map<int64_t, int64_t> new_stones;
            for (const auto& [stone, count] : stones) {
                if (stone == 0) {
                    new_stones[1] += count;
                    continue;
                }

                int digit_count = static_cast<int>(std::floor(std::log10(stone))) + 1;
                if (digit_count % 2) {
                    if (stone > 4557001994493466) {
                        throw std::domain_error("overflow");
                    }
                    new_stones[stone * 2024] += count;
                    continue;
                }

                int64_t divisor = static_cast<int64_t>(std::pow(10, digit_count / 2));
                new_stones[stone / divisor] += count;
                new_stones[stone % divisor] += count;
            }
            stones = new_stones;
        }

        int64_t result{0};

        for (const auto& [_, count] : stones) {
            result += count;
        }

        return result;
    }

    std::map<int64_t, int64_t> parse(const std::string& row) {
        std::map<int64_t, int64_t> result;

        for (auto stone : advent::ints<int64_t>(row)) {
            result[stone]++;
        }

        return result;
    }
}
