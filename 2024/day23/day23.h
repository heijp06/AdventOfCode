#pragma once

#include <set>
#include <string>
#include <unordered_map>
#include <vector>

namespace day23 {
    int part1(const std::vector<std::string>& rows);
    std::string part2(const std::vector<std::string>& rows);

    std::vector<std::set<std::string>> get_triples(std::unordered_map<std::string, std::vector<std::string>> pairs);
    std::unordered_map<std::string, std::vector<std::string>> get_pairs(const std::vector<std::string>& rows);
}
