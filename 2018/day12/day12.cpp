#include <algorithm>
#include <numeric>
#include <set>

#include "day12.h"

namespace day12 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        return solve(rows, false);
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        return solve(rows, true);
    }

    std::int64_t solve(const std::vector<std::string>& rows, bool part2) {
        const auto& clues = parse_clues(rows);

        std::set<std::int64_t> current{};

        const int offset = 15;

        for (size_t i = offset; i < rows[0].size(); i++) {
            if (rows[0][i] == '#') {
                current.insert(i - offset);
            }
        }

        std::set<std::int64_t> next{};

        for (size_t i = 0; part2 || i < 20; i++) {
            std::int64_t min = *current.cbegin() - 2;
            std::int64_t max = *current.crbegin() + 2;
            int key{};
            for (std::int64_t pot = min; pot <= max; pot++) {
                key >>= 1;

                if (current.count(pot + 2)) {
                    key |= 16;
                }

                if (clues[key]) {
                    next.insert(pot);
                }
            }

            if (part2) {
                if (current.size() == next.size()) {
                    if (std::all_of(current.cbegin(), current.cend(), [&](std::int64_t pot) { return next.count(pot + 1); })) {
                        std::int64_t sum = std::reduce(current.cbegin(), current.cend());
                        std::int64_t todo = 50'000'000'000 - i;
                        std::int64_t count = current.size();
                        return sum + todo * count;
                    }
                }
            }

            std::swap(current, next);
            next.clear();
        }

        return std::reduce(current.cbegin(), current.cend());
    }

    std::vector<bool> parse_clues(const std::vector<std::string>& rows) {
        std::vector<bool> clues(32, false);

        for (auto& it = rows.cbegin() + 2; it < rows.cend(); it++) {
            const auto& row = *it;
            int clue = 0;
            int bit = 1;
            for (size_t i = 0; i < 5; i++) {
                if (row[i] == '#') {
                    clue += bit;
                }
                bit <<= 1;
            }
            clues[clue] = row[9] == '#';
        }

        return clues;
    }
}
