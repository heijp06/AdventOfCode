#include <sstream>

#include "day01.h"

namespace day01 {
    int part1(std::vector<std::string> rows) {
        int sum = 0;

        for (auto value : changes(rows)) {
            sum += value;
        }

        return sum;
    }

    int part2(std::vector<std::string> rows) {
        (void)rows;
        return -1;
    }

    std::vector<int> changes(std::vector<std::string> rows) {
        std::vector<int> result{};

        for (auto row : rows) {
            int value;
            std::istringstream rs{row};
            rs >> value;
            result.push_back(value);
        }

        return result;
    }
}
