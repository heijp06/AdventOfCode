#include <cstdint>
#include <utility>

#include "day05.h"
#include "../../lib/advent.h"

using range = std::pair<std::int64_t, std::int64_t>;

namespace day05 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<range> ranges{};
        std::vector<std::int64_t> ingredients;

        for (const auto& row : rows) {
            const auto& fields = advent::ints<int64_t>(row);

            if (fields.size() == 1) {
                ingredients.push_back(fields[0]);
            }

            if (fields.size() == 2) {
                ranges.push_back(std::make_pair(fields[0], -fields[1]));
            }
        }

        auto result{0};

        for (const auto& ingredient : ingredients) {
            for (const auto& range : ranges) {
                if (range.first <= ingredient && range.second >= ingredient) {
                    result++;
                    break;
                }
            }
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
