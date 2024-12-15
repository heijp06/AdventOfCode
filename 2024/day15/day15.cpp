#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        auto it = empty_row(rows);

        auto& grid = advent::grid(std::vector<std::string>(rows.cbegin(), it));

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<std::string>::const_iterator empty_row(const std::vector<std::string>& rows) {
        auto it = rows.cbegin();

        while (!it->empty()) {
            ++it;
        }

        return it;
    }
}
