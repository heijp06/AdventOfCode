#include "day09.h"

namespace day09 {
    int part1(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<int> parse(std::string row) {
        std::vector<int> result;

        for (const auto c : row) {
            result.push_back(c - '0');
        }

        return result;
    }
}
