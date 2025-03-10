#include <algorithm>
#include <cctype>

#include "day05.h"

namespace day05 {
    int part1(const std::vector<std::string>& rows) {
        const auto& polymer = rows.at(0);
        return solve(polymer);
    }

    int part2(const std::vector<std::string>& rows) {
        const auto& polymer = rows.at(0);
        auto min_length{static_cast<int>(polymer.size())};

        for (char c = 'a'; c <= 'z'; c++) {
            min_length = std::min(min_length, solve(polymer, c));
        }

        return min_length;
    }

    int solve(const std::string& polymer, const char skip) {
        std::string result;

        for (const auto unit : polymer) {
            if (unit == skip || std::tolower(unit) == std::tolower(skip)) {
                continue;
            }

            if (!result.empty()) {
                const auto back = result.back();
                if (back != unit && std::tolower(back) == std::tolower(unit)) {
                    result.pop_back();
                    continue;
                }
            }

            result.push_back(unit);
        }

        return static_cast<int>(result.size());
    }
}
