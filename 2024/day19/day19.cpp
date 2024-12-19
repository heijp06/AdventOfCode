#include "day19.h"
#include "../../lib/advent.h"

namespace day19 {
    constexpr int six_pow_eight = 36 * 36 * 36 * 36;

    int part1(const std::vector<std::string>& rows) {
        const auto& towels = advent::split(rows[0], ", ");
        const auto& towel_codes = get_towel_codes(towels);
        const auto& designs = std::vector<std::string>(rows.cbegin() + 2, rows.cend());

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<bool> get_towel_codes(const std::vector<std::string>& towels) {
        auto towel_codes = std::vector<bool>(six_pow_eight);
        for (const auto& towel : towels) {
            int towel_code = 0;
            for (char c : towel) {
                int color_code = get_color_code(c);
                towel_code = towel_code * 6 + color_code;
            }
            towel_codes[towel_code] = true;
        }
        return towel_codes;
    }

    int get_color_code(const char c) {
        const std::string& color_codes = "wubrg";
        return static_cast<int>(color_codes.find(c)) + 1;
    }
}
