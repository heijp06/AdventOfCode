#include <algorithm>
#include "day03.h"

namespace day03 {
    int part1(const std::vector<std::string>& rows) {
        auto sum{0};

        for (const auto& row : rows) {
            sum += joltage(row);
        }

        return sum;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int joltage(const std::string& row) {
        const auto& d1 = *std::max_element(row.cbegin(), row.cend() - 1);
        const auto& pos = std::find(row.cbegin(), row.cend(), d1);
        const auto& d2 = *std::max_element(pos + 1, row.cend());

        return (d1 - '0') * 10 + d2 - '0';
    }
}
