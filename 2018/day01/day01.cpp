#include <set>
#include <sstream>

#include "day01.h"

namespace day01 {
    int part1(const std::vector<std::string>& rows) {
        int sum = 0;

        for (auto value : changes(rows)) {
            sum += value;
        }

        return sum;
    }

    int part2(const std::vector<std::string>& rows) {
        int sum = 0;
        auto nums{changes(rows)};
        std::set<int> seen{0};

        while (true) {
            for (auto num : nums) {
                sum += num;
                if (!seen.insert(sum).second) {
                    return sum;
                }
            }
        }

        return -1;
    }

    std::vector<int> changes(const std::vector<std::string>& rows) {
        std::vector<int> result{};

        for (const auto& row : rows) {
            int value;
            std::istringstream rs{row};
            rs >> value;
            result.push_back(value);
        }

        return result;
    }
}
