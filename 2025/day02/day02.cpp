#include <numeric>
#include <cmath>
#include <regex>
#include <set>

#include "../../lib/advent.h"
#include "day02.h"

namespace day02 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t sum{0};

        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& nums = advent::ints<std::int64_t>(pair);
            sum += sum_invalid(nums[0], -nums[1], false);
        }

        return sum;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        std::int64_t sum{0};

        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& nums = advent::ints<std::int64_t>(pair);
            sum += sum_invalid(nums[0], -nums[1], true);
        }

        return sum;
    }

    std::int64_t sum_invalid(const std::int64_t& num_start, const std::int64_t& num_end, const bool part2)
    {
        std::set<std::int64_t> invalid;

        const auto& start = std::to_string(num_start);
        const auto& end = std::to_string(num_end);

        auto stop = part2 ? static_cast<int>(end.length()) : 2;

        for (int i = 2; i <= stop; i++) {
            if (start.length() % i && end.length() % i) {
                continue;
            }

            int len = start.length() % i ? end.length() / i : start.length() / i;
            auto power = pow(10, len);
            auto div = pow(power, i - 1);
            std::int64_t val = start.length() % i ? power / 10 : num_start / div;

            std::int64_t num = rep(val++, i, power);
            while (num <= num_end && val <= power) {
                if (num >= num_start) {
                    invalid.insert(num);
                }
                num = rep(val++, i, power);
            }
        }

        return std::reduce(invalid.cbegin(), invalid.cend());
    }

    std::int64_t pow(const std::int64_t& base, int power) {
        std::int64_t result{1};

        for (int i = 0; i < power; i++) {
            result *= base;
        }
        return result;
    }

    std::int64_t rep(const std::int64_t& val, int repetition, const std::int64_t& power) {
        std::int64_t result{0};

        for (int i = 0; i < repetition; i++) {
            result *= power;
            result += val;
        }

        return result;
    }
}
