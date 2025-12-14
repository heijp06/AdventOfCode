#include <numeric>
#include "day12.h"
#include "../../lib/advent.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows) {
        int result{};

        for (const auto& row : rows) {
            if (row.size() < 3 || row[2] != 'x') {
                continue;
            }

            const auto& nums = advent::ints(row);
            result += nums[0] * nums[1] / 9 >= std::reduce(nums.cbegin() + 2, nums.cend());
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
