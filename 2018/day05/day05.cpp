#include <cctype>

#include "day05.h"

namespace day05 {
    int part1(const std::vector<std::string>& rows) {
        std::string result;
        const auto& polymer = rows.at(0);

        for (const auto unit : polymer) {
            if (!result.empty()) {
                const auto back = result.back();
                if (back != unit && std::tolower(back) == std::tolower(unit)) {
                    result.pop_back();
                    continue;
                }
            }
            result.push_back(unit);
        }

        return static_cast<int>(result.size());
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
