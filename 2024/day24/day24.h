#pragma once

#include <optional>
#include <set>

#include <string>
#include <unordered_map>
#include <vector>

namespace day24 {
    enum class operations {AND, OR, XOR};

    struct gate {
        std::string input1;
        std::string input2;
        std::string output;
        operations operation;
    };

    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::vector<gate> parse_gates(const std::vector<std::string>& rows);
    std::unordered_map<std::string, bool> parse_known_wires(const std::vector<std::string>& rows);
    std::set<std::string> get_z_wires(const std::vector<gate>& gates);
    std::optional<bool> get_output(operations operation, std::optional<bool> input1, std::optional<bool> input2);
}
