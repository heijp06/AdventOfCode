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

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
