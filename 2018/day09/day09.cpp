#include <algorithm>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        return solve(rows, 1);
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        return solve(rows, 100);
    }

    std::int64_t solve(const std::vector<std::string>& rows, std::int64_t multiplier) {
        const auto& fields = advent::ints<std::int64_t>(rows.front());

        std::int64_t players = fields[0];
        std::int64_t last = fields[1] * multiplier;

        std::vector<std::int64_t> scores(players, 0);

        auto current = std::make_shared<Node>();
        current->value = 0;
        current->next = current;
        current->previous = current;

        std::int64_t current_player{};
        for (std::int64_t marble = 1; marble <= last; marble++) {
            if (marble % 23) {
                auto to_add = std::make_shared<Node>();
                auto& where = current->next;

                to_add->value = marble;
                to_add->next = where->next;
                to_add->previous = where;

                where->next->previous = to_add;
                where->next = to_add;

                current = to_add;
            }
            else {
                auto& to_remove = current;
                for (size_t i = 0; i < 7; i++) {
                    to_remove = to_remove->previous;
                }

                scores[current_player] += marble + to_remove->value;

                to_remove->previous->next = to_remove->next;
                to_remove->next->previous = to_remove->previous;

                current = to_remove->next;
            }

            current_player = (current_player + 1) % players;
        }

        return *std::max_element(scores.cbegin(), scores.cend());
    }
}
