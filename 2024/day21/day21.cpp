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

    std::vector<std::map<std::string, std::string>> get_directional_moves() {
        std::map<std::string, std::vector<std::string>> mapping = {
            {"AA", {"A"}}, {"A^", {"<A"}}, {"A>", {"vA"}}, {"Av", {"<vA", "v<A"}}, {"A<", {"<v<A", "v<<A"}},
            {"^A", {">A"}}, {"^^", {"A"}}, {"^>", {">vA", "v>A"}}, {"^v", {"vA"}}, {"^<", {"v<A"}},
            {">A", {"^A"}}, {">^", {"^<A", "<^A"}}, {">>", {"A"}}, {">v", {"<A"}}, {"><", {"<<A"}},
            {"vA", {">^A", "^>A"}}, {"v^", {"^A"}}, {"v>", {">A"}}, {"vv", {"A"}}, {"v<", {"<A"}},
            {"<A", {">^>A", ">>^A"}}, {"<^", {">^A"}}, {"<>", {">>A"}}, {"<v", {">A"}}, {"<<", {"A"}}
        };

        std::vector<std::map<std::string, std::string>> result{{}};

        for (const auto& [key, options] : mapping) {
            std::vector<std::map<std::string, std::string>> new_result{};
            for (const auto& dict : result) {
                for (const auto& option : options) {
                    auto new_dict = dict;
                    new_dict[key] = option;
                    new_result.push_back(new_dict);
                }
            }
            result = new_result;
        }

        return result;
    }
}
