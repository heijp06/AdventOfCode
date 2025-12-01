#include "day01.h"

namespace day01 {
    int part1(const std::vector<std::string>& rows) {
        auto result{0};
        auto sum{50};
        for (const auto& row : rows) {
            const auto number = std::stoi(row.substr(1));
            if (row[0] == 'L') {
                sum -= number;
            }
            else {
                sum += number;
            }
            result += sum % 100 == 0;
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
