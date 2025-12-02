#include <cmath>

#include "../../lib/advent.h"
#include "day02.h"

namespace day02 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t sum{0};

        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& nums = advent::ints<std::int64_t>(pair);
            sum += sum_invalid(nums[0], std::abs(nums[1]));
        }

        return sum;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::int64_t sum_invalid(const std::int64_t& num_start, const std::int64_t& num_end)
    {
        const auto& start = std::to_string(num_start);
        const auto& end = std::to_string(num_end);

        if (start.length() % 2 && end.length() % 2) {
            return 0;
        }

        const auto& length = start.length() % 2
            ? static_cast<std::int64_t>(end.length() / 2)
            : static_cast<std::int64_t>(start.length() / 2);
        const auto& pow = static_cast<std::int64_t>(std::pow(10, length - 1));
        auto min = start.length() % 2 ? pow : advent::ints<std::int64_t>(start.substr(0, length))[0];
        auto max = end.length() % 2 ? pow * 10 - 1 : advent::ints<std::int64_t>(end.substr(0, length))[0];

        auto min_val = min * pow * 10 + min;
        if (min_val < num_start) {
            min_val += pow * 10 + 1;
            min++;
            if (min_val > num_end) {
                return 0;
            }
        }

        auto max_val = max * pow * 10 + max;
        if (max_val > num_end) {
            max_val -= pow * 10 + 1;
            max--;
            if (max_val < num_start) {
                return 0;
            }
        }

        return (max - min + 1) * (max + min) / 2 * (pow * 10 + 1);
    }
}
