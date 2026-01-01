#include <numeric>
#include <set>

#include "day12.h"

namespace day12 {
    int part1(const std::vector<std::string>& rows) {
        const int offset = 15;
        std::set<int> current{};
        std::vector<bool> clues(32, false);

        for (size_t i = offset; i < rows[0].size(); i++) {
            if (rows[0][i] == '#') {
                current.insert(i - offset);
            }
        }

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

        std::set<int> next{};

        for (size_t i = 0; i < 20; i++) {
            int min = *current.cbegin() - 2;
            int max = *current.crbegin() + 2;
            int key{};
            for (int pot = min; pot <= max; pot++) {
                key >>= 1;

                if (current.count(pot + 2)) {
                    key |= 16;
                }

                if (clues[key]) {
                    next.insert(pot);
                }
            }
            std::swap(current, next);
            next.clear();
        }

        return std::reduce(current.cbegin(), current.cend());
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
