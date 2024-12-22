#include <utility>

#include "day21.h"

namespace day21 {
    int part1(const std::vector<std::string>& rows) {
        const auto& directional_moves = get_directional_moves();

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::map<std::string, std::string> get_directional_moves() {
        std::map<std::string, std::vector<std::string>> mapping = {
            {"AA", {"A"}}, {"A^", {"<A"}}, {"A>", {"vA"}}, {"Av", {"<vA", "v<A"}}, {"A<", {"<v<A", "v<<A"}},
            {"^A", {">A"}}, {"^^", {"A"}}, {"^>", {">vA", "v>A"}}, {"^v", {"vA"}}, {"^<", {"v<A"}},
            {">A", {"^A"}}, {">^", {"^<A", "<^A"}}, {">>", {"A"}}, {">v", {"<A"}}, {"><", {"<<A"}},
            {"vA", {">^A", "^>A"}}, {"v^", {"^A"}}, {"v>", {">A"}}, {"vv", {"A"}}, {"v<", {"<A"}},
            {"<A", {">^>A", ">>^A"}}, {"<^", {">^A"}}, {"<>", {">>A"}}, {"<v", {">A"}}, {"<<", {"A"}}
        };

        return std::map<std::string, std::string>();
    }
}
