#include <functional>
#include <numeric>

#include "day06.h"
#include "../../lib/advent.h"

namespace day06 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::vector<std::vector<std::int64_t>> numbers{};
        std::vector<char> operators{};

        for (const auto& row : rows) {
            const auto& ints = advent::ints<std::int64_t>(row);
            if (ints.empty()) {
                const auto& split = advent::split(row, " ");
                for (const auto& op : split) {
                    if (!op.empty()) {
                        operators.emplace_back(op[0]);
                    }
                }
            }
            else {
                numbers.emplace_back(ints);
            }
        }

        std::int64_t total{};

        for (size_t i = 0; i < operators.size(); i++) {
            const auto& op = operators[i];
            std::int64_t result = op == '+' ? 0 : 1;
            for (const auto& ints : numbers) {
                if (op == '+') {
                    result += ints[i];
                }
                else {
                    result *= ints[i];
                }
            }
            total += result;
        }

        return total;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        std::int64_t total{};
        const auto& grid = advent::grid(rows);

        std::vector<std::int64_t> numbers;
        for (int column = grid.get_width() - 1; column >= 0; column--) {
            std::int64_t number{};
            for (int row = 0; row < grid.get_height(); row++) {
                auto c = grid[advent::coord{row, column}];
                if (c == ' ') {
                    continue;
                }

                if (c == '+') {
                    numbers.push_back(number);
                    total += std::reduce(numbers.cbegin(), numbers.cend());
                    numbers.clear();
                    number = 0;
                    continue;
                }

                if (c == '*') {
                    numbers.push_back(number);
                    total += std::reduce(numbers.cbegin(), numbers.cend(), UINT64_C(1), std::multiplies<std::int64_t>());
                    numbers.clear();
                    number = 0;
                    continue;
                }

                number = number * 10 + c - '0';
            }
            if (number > 0) {
                numbers.push_back(number);
            }
        }

        return total;
    }
}
