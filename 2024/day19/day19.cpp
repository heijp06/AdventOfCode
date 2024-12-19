#include "day19.h"
#include "../../lib/advent.h"

namespace day19 {
    constexpr int six_pow_eight = 36 * 36 * 36 * 36;

    int part1(const std::vector<std::string>& rows) {
        const auto& towels = advent::split(rows[0], ", ");
        const auto& designs = std::vector<std::string>(rows.cbegin() + 2, rows.cend());
        const std::string& color_codes = "wubrg";

        auto table = std::vector<bool>(six_pow_eight);
        for (const auto& towel : towels) {
            int towel_code = 0;
            for (char c : towel) {
                int color_code = static_cast<int>(color_codes.find(c)) + 1;
                towel_code = towel_code * 6 + color_code;
            }
            table[towel_code] = true;
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
