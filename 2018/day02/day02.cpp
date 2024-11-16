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

    std::string part2(std::vector<std::string> rows) {
        for (size_t i = 0; i < rows.size() - 1; i++) {
            auto row1{rows.at(i)};
            for (size_t j = i + 1; j < rows.size(); j++) {
                auto row2{rows.at(j)};
                auto diff{0};

                for (size_t k = 0; k < row1.size(); k++) {
                    diff += row1.at(k) != row2.at(k);
                }

                if (diff != 1) {
                    continue;
                }

                std::string result{};
                for (size_t k = 0; k < row1.size(); k++) {
                    if (row1.at(k) == row2.at(k)) {
                        result += row1.at(k);
                    }
                }
                return result;
            }
        }

        return "";
    }
}
