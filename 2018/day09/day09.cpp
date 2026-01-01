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
        }

        return current->next->value;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
