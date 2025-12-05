#include <cstdint>
#include <utility>

#include "day05.h"
#include "../../lib/advent.h"

namespace day05 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<range_t> ranges{};
        std::vector<std::int64_t> ingredients;

        parse(rows, ingredients, ranges);

        auto result{0};

        for (const auto& ingredient : ingredients) {
            for (const auto& range_t : ranges) {
                if (range_t.first <= ingredient && range_t.second >= ingredient) {
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

    void parse(const std::vector<std::string>& rows, std::vector<int64_t>& ingredients, std::vector<range_t>& ranges) {
        for (const auto& row : rows) {
            const auto& fields = advent::ints<int64_t>(row);

            if (fields.size() == 1) {
                ingredients.push_back(fields[0]);
            }

            if (fields.size() == 2) {
                ranges.push_back(std::make_pair(fields[0], -fields[1]));
            }
        }
    }

    void merge(const range_t& range, std::vector<range_t>& ranges) {
        ranges.push_back(range);
    }
}
