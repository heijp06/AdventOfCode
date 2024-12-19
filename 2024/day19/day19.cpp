#include <set>

#include "day19.h"
#include "../../lib/advent.h"

namespace day19 {
    constexpr int six_pow_eight = 36 * 36 * 36 * 36;

    int part1(const std::vector<std::string>& rows) {
        const auto& towels = advent::split(rows[0], ", ");
        const auto& towel_codes = get_towel_codes(towels);
        const auto& designs = std::vector<std::string>(rows.cbegin() + 2, rows.cend());

        int result = 0;

        for (const auto& design : designs) {
            auto size = static_cast<int>(design.size());
            auto indices = std::vector<int>({-1});
            std::set<int> seen;
            auto found{false};
            while (!indices.empty() && !found) {
                std::vector<int> new_indices;
                for (const int index : indices) {
                    int towel_code = 0;
                    for (int offset = 1; offset <= 8; offset++) {
                        auto new_index = index + offset;
                        if (new_index >= size || found) {
                            break;
                        }
                        towel_code = 6 * towel_code + get_color_code(design[new_index]);
                        if (towel_codes[towel_code]) {
                            if (new_index == size - 1) {
                                found = true;
                            }
                            else if (!seen.count(new_index)) {
                                seen.insert(new_index);
                                new_indices.push_back(new_index);
                            }
                        }
                    }
                    if (found) {
                        break;
                    }
                }
                indices = new_indices;
            }
            result += found;
        }

        return result;
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
