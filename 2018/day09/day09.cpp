#include <algorithm>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    int part1(const std::vector<std::string>& rows) {
        const auto& fields = advent::ints(rows.front());

        int players = fields[0];
        int last = fields[1];

        std::vector<int> scores(players, 0);

        auto current = std::make_shared<Node>();
        current->value = 0;
        current->next = current;
        current->previous = current;

        int current_player{};
        for (int marble = 1; marble <= last; marble++) {
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

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
