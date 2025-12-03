#include <algorithm>
#include "day03.h"

namespace day03 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        return calculate_sum(rows, 2);
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        return calculate_sum(rows, 12);
    }

    int64_t calculate_sum(const std::vector<std::string>& rows, int digits) {
        int64_t sum{0};

        for (const auto& row : rows) {
            sum += joltage(row, digits);
        }

        return sum;
    }

    std::int64_t joltage(const std::string& row, int digits) {
        std::int64_t joltage{0};
        auto pos = row.begin();

        for (int i = 0; i < digits; i++) {
            pos = std::max_element(pos, row.cend() - digits + i + 1);
            joltage *= 10;
            joltage += *pos - '0';
            pos++;
        }

        return joltage;
    }
}
