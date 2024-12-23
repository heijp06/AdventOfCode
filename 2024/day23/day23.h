#pragma once

#include <string>
#include <vector>

namespace day23 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<std::vector<std::string>> get_triples(const std::vector<std::string>& rows);
}
