#include <utility>

#include "day21.h"

namespace day21 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t result{0};
        const auto& directional_moves = get_directional_moves();

        for (const auto& row : rows) {
            const std::string& numerical_move = "A" + row;
            for (size_t i = 0; i < numerical_move.size() - 1; i++) {
                std::int64_t min_moves{-1};
                for (const auto& move : get_numerical_moves(numerical_move[i], numerical_move[i + 1])) {
                    for (const auto& mapping : directional_moves) {
                        const auto moves = get_moves(move, mapping, 2);
                    }
                }
            }
        }

        return result;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
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

    std::int64_t get_moves(const std::string& move, const std::map<std::string, std::string>& mapping, int count) {
        std::map<std::string, std::int64_t> key_strokes;
        //std::string first_move = "A" + move.substr(0, 1);
        //key_strokes[first_move] = 1;
        std::string actual_move = "A" + move.substr(0, 1);
        for (size_t i = 0; i < actual_move.size() - 1; i++) {
            key_strokes[actual_move.substr(i, 2)]++;
        }

        for (size_t i = 0; i < count; i++) {
            std::map<std::string, std::int64_t> new_key_strokes;
            //first_move = ("A" + mapping.at(first_move)).substr(0, 2);
            //new_key_strokes[first_move] = 1;
            for (const auto& [key_stroke, count] : key_strokes) {
                const auto& directional_move = mapping.at(key_stroke);
                for (size_t j = 0; j < directional_move.size() - 1; j++) {
                    new_key_strokes[directional_move.substr(j, 2)] += count;
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
