#include "day19.h"
#include "../../lib/advent.h"

namespace day19 {
    int part1(const std::vector<std::string>& rows) {
        const auto& towels = advent::split(rows[0], ", ");
        const auto& designs = std::vector<std::string>(rows.cbegin() + 2, rows.cend());

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
