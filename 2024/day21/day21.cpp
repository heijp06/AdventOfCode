#include <cstdint>
#include <utility>

#include "day21.h"

namespace day21 {
    int part1(const std::vector<std::string>& rows) {
        const auto& directional_moves = get_directional_moves();

        for (const auto& row : rows) {
            for (size_t i = 0; i < row.size() - 1; i++) {
                std::int64_t min_moves{-1};
                for (const auto& move : get_numerical_moves(row[i], row[i + 1])) {
                    for (const auto& mapping : directional_moves) {
                        // call func set new min.
                    }
                }
            }
        }

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

    std::vector<std::string> get_numerical_moves(const char from, const char to) {
        advent::coord empty = {3, 0};
        auto current = get_position(from);
        auto target = get_position(to);

        std::vector<std::pair<advent::coord, std::string>> results = {{current, ""}};
        auto stop{false};

        while (!stop) {
            std::vector<std::pair<advent::coord, std::string>> new_results;
            for (const auto& result : results) {
                if (result.first == target) {
                    new_results.push_back({result.first, result.second + "A"});
                    stop = true;
                    continue;
                }

                if (result.first.column < target.column) {
                    new_results.push_back({result.first + advent::direction::right(), result.second + ">"});
                }

                if (result.first.column > target.column && result.first + advent::direction::left() != empty) {
                    new_results.push_back({result.first + advent::direction::left(), result.second + "<"});
                }

                if (result.first.row < target.row && result.first + advent::direction::down() != empty) {
                    new_results.push_back({result.first + advent::direction::down(), result.second + "v"});
                }

                if (result.first.row > target.row) {
                    new_results.push_back({result.first + advent::direction::up(), result.second + "^"});
                }
            }
            results = new_results;
        }

        std::vector<std::string> moves;
        for (const auto& result : results) {
            moves.push_back(result.second);
        }

        return moves;
    }

    advent::coord get_position(const char c) {
        if (c == 'A') {
            return {3, 2};
        }

        if (c == '0') {
            return {3, 1};
        }

        return {3 - (c - '0' + 2) / 3, (c - '0' + 2) % 3};
    }
}
