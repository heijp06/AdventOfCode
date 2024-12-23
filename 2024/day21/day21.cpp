#include <utility>

#include "day21.h"

namespace day21 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        return solve(rows, 2);
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        return solve(rows, 25);
    }

    int64_t solve(const std::vector<std::string>& rows, int times) {
        std::int64_t result{0};
        const auto& directional_moves = get_directional_moves();

        for (const auto& row : rows) {
            const std::string& numerical_move = "A" + row;
            std::int64_t total = 0;
            for (size_t i = 0; i < numerical_move.size() - 1; i++) {
                std::int64_t min_moves{-1};
                for (const auto& move : get_numerical_moves(numerical_move[i], numerical_move[i + 1])) {
                    for (const auto& mapping : directional_moves) {
                        const auto& moves = get_moves(move, mapping, times);
                        if (min_moves == -1 || moves < min_moves) {
                            min_moves = moves;
                        }
                    }
                }
                total += min_moves;
            }
            result += total * advent::ints<std::int64_t>(row)[0];
        }

        return result;
    }

    std::vector<std::map<std::string, std::string>> get_directional_moves() {
        std::map<std::string, std::vector<std::string>> mapping = {
            {"AA", {""}}, {"A^", {"<"}}, {"A>", {"v"}}, {"Av", {"<v", "v<"}}, {"A<", {"<v<", "v<<"}},
            {"^A", {">"}}, {"^^", {""}}, {"^>", {">v", "v>"}}, {"^v", {"v"}}, {"^<", {"v<"}},
            {">A", {"^"}}, {">^", {"^<", "<^"}}, {">>", {""}}, {">v", {"<"}}, {"><", {"<<"}},
            {"vA", {">^", "^>"}}, {"v^", {"^"}}, {"v>", {">"}}, {"vv", {""}}, {"v<", {"<"}},
            {"<A", {">^>", ">>^"}}, {"<^", {">^"}}, {"<>", {">>"}}, {"<v", {">"}}, {"<<", {""}}
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
                    new_results.push_back({result.first, result.second + ""});
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

    std::int64_t get_moves(const std::string& move, const std::map<std::string, std::string>& mapping, int times) {
        std::map<std::string, std::int64_t> key_strokes;
        if (move.size()) {
            key_strokes[std::string("A") + move[0]]++;
            for (size_t i = 0; i < move.size() - 1; i++) {
                key_strokes[move.substr(i, 2)]++;
            }
            key_strokes[move.back() + std::string("A")]++;
        }
        else {
            key_strokes[std::string("AA")]++;
        }

        for (int i = 0; i < times; i++) {
            std::map<std::string, std::int64_t> new_key_strokes;
            for (const auto& [key_stroke, count] : key_strokes) {
                const auto& directional_move = mapping.at(key_stroke);
                if (directional_move.size()) {
                    new_key_strokes[std::string("A") + directional_move[0]] += count;
                    for (size_t j = 0; j < directional_move.size() - 1; j++) {
                        new_key_strokes[directional_move.substr(j, 2)] += count;
                    }
                    new_key_strokes[directional_move.back() + std::string("A")] += count;
                }
                else {
                    new_key_strokes[std::string("AA")] += count;
                }
            }
            key_strokes = new_key_strokes;
        }

        std::int64_t result{0};
        for (const auto& [_, count] : key_strokes) {
            result += count;
        }

        return result;
    }
}
