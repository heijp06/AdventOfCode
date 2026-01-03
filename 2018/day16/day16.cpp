#include "day16.h"
#include "../../lib/advent.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows) {
        int result{};

        for (auto it = rows.cbegin(); it != rows.cend(); ++it) {
            if (it->empty()) {
                break;
            }

            const auto& before = advent::ints(*it++);
            const auto& instruction = advent::ints(*it++);
            const auto& after = advent::ints(*it++);
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
