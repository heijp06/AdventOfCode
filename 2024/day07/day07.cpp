#include <algorithm>
#include <cmath>
#include <set>

#include "day07.h"
#include "../../lib/advent.h"

namespace day07 {
    int64_t part1(const std::vector<std::string>& rows) {
        int64_t result{0};

        for (const auto& row : rows) {
            const auto& equation = advent::longs(row);
            if (solve(equation)) {
                result += equation[0];
            }
        }

        return result;
    }

    int64_t part2(const std::vector<std::string>& rows) {
        int64_t result{0};

        for (const auto& row : rows) {
            const auto& equation = advent::longs(row);
            if (solve(equation, true)) {
                result += equation[0];
            }
        }

        return result;
    }

    bool solve(const std::vector<int64_t>& equation, bool part2) {
        int64_t sum = equation[0];
        auto sums = std::vector<int64_t>{0};

        for (int i = 1; i < static_cast<int>(equation.size()); i++) {
            const int64_t value = equation[i];
            std::vector<int64_t> new_sums;
            for (const auto item : sums) {
                auto result = item + value;
                if (result <= sum) {
                    new_sums.push_back(result);
                }
                result = item * value;
                if (result <= sum) {
                    new_sums.push_back(result);
                }
                if (part2) {
                    result = item == 0 ? value : concatenate(item, value);
                    if (result <= sum) {
                        new_sums.push_back(result);
                    }
                }
            }
            std::swap(sums, new_sums);
        }

        return std::find(sums.cbegin(), sums.cend(), sum) != sums.cend();
    }

    int64_t concatenate(const int64_t left, const int64_t right) {
        int exponent = std::log10(right);
        return static_cast<int>(std::pow(10, exponent + 1)) * left + right;
    }
}