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

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    bool solve(const std::vector<int64_t>& equation) {
        int64_t sum = equation[0];
        auto sums = std::set<int64_t>{0};

        for (int i = 1; i < equation.size(); i++) {
            const int64_t value = equation[i];
            std::set<int64_t> new_sums;
            for (const auto item : sums) {
                auto result = value + item;
                if (result <= sum) {
                    new_sums.insert(result);
                }
                result = value * item;
                if (result <= sum) {
                    new_sums.insert(result);
                }
            }
            std::swap(sums, new_sums);
        }

        return sums.count(sum);
    }
}
