#pragma once

#include <string>
#include <vector>

namespace day01 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int get_password(const std::vector<std::string>& rows, bool part2 = false);
}
