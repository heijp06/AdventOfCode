#pragma once

#include <string>
#include <vector>

namespace day19 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int create_design(const std::string& design, const std::vector<bool>& towel_codes);
    std::vector<bool> get_towel_codes(const std::vector<std::string>& towels);
    int get_color_code(const char c);
}
