#include <cmath>
#include <regex>

#include "../../lib/advent.h"
#include "day02.h"

namespace day02 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t sum{0};

        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& nums = advent::ints<std::int64_t>(pair);
            sum += sum_invalid(nums[0], std::abs(nums[1]), "(.*)\\1");
        }

        return sum;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        std::int64_t sum{0};

        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& nums = advent::ints<std::int64_t>(pair);
            sum += sum_invalid(nums[0], std::abs(nums[1]), "(.*)(\\1)+");
        }

        return sum;
    }

    std::int64_t sum_invalid(const std::int64_t& num_start, const std::int64_t& num_end, const std::string& regex)
    {
        const std::regex txt_regex(regex);
        std::int64_t sum{0};

        for (auto i = num_start; i <= num_end; i++) {
            if (std::regex_match(std::to_string(i), txt_regex)) {
                sum += i;
            }
        }

        return sum;
    }
}
