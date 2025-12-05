#include <algorithm>
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

    std::int64_t part2(const std::vector<std::string>& rows) {
        std::vector<range_t> ranges{};
        std::vector<std::int64_t> ingredients;

        parse(rows, ingredients, ranges);

        ranges = merge(ranges);

        std::int64_t result{0};

        for (const auto& range : ranges) {
            result += range.second - range.first + 1;
        }

        return result;
    }

    void parse(const std::vector<std::string>& rows, std::vector<int64_t>& ingredients, std::vector<range_t>& ranges) {
        for (const auto& row : rows) {
            const auto& fields = advent::ints<int64_t>(row);

            if (fields.size() == 1) {
                ingredients.push_back(fields[0]);
            }

            if (fields.size() == 2) {
                ranges.emplace_back(std::make_pair(fields[0], -fields[1]));
            }
        }
    }

    std::vector<range_t> merge(const std::vector<range_t>& ranges)
    {
        std::vector<range_t> merged;
        merged.reserve(ranges.size());
        std::vector<range_t> merging;
        merging.reserve(ranges.size());

        for (auto to_merge : ranges) {
            bool done = false;
            for (const auto& range : merged) {
                if (done || range.second < to_merge.first - 1) {
                    merging.emplace_back(range);
                    continue;
                }

                if (range.first > to_merge.second + 1) {
                    done = true;
                    merging.emplace_back(to_merge);
                    merging.emplace_back(range);
                    continue;
                }

                to_merge = std::make_pair(std::min(to_merge.first, range.first), std::max(to_merge.second, range.second));
            }
            if (!done) {
                merging.emplace_back(to_merge);
            }
            std::swap(merged, merging);
            merging.clear();
        }

        return merged;
    }
}
