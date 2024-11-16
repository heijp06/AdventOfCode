#include "day02.h"

#include <map>

namespace day02 {
    int part1(std::vector<std::string> rows) {
        auto twice{0};
        auto thrice{0};

        for (const auto& row : rows) {
			std::map<char, int> counts{};
            for (auto c : row) {
                counts[c]++;
            }

            auto has_twice{false};
            auto has_thrice{false};
            for (auto it = counts.cbegin(); it != counts.cend(); it++) {
                if (it->second == 2) {
                    has_twice = true;
                }
                else if (it->second == 3) {
                    has_thrice = true;
                }
            }
            twice += has_twice;
            thrice += has_thrice;
        }

        return twice * thrice;
    }

    int part2(std::vector<std::string> rows) {
        (void)rows;
        return -1;
    }
}
