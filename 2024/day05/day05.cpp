#include <set>
#include <utility>

#include "day05.h"
#include "../../lib/advent.h"

namespace day05 {
    int part1(const std::vector<std::string>& rows) {
        std::set<std::pair<int, int>> rules;
        std::vector<std::vector<int>> updates;
        auto parse_rules{true};

        for (const auto& row: rows) {
	        if (row.empty()) {
                parse_rules = false;
                continue;
	        }

            if (parse_rules) {
                const auto& numbers = advent::ints(row);
                rules.insert(std::make_pair(numbers[0], numbers[1]));
                continue;
            }

            updates.push_back(advent::ints(row));
        }

        auto result{0};
		for (const auto& update: updates) {
            auto correct{true};
            for (size_t i = 0; i < update.size() - 1; i++) {
                const auto& pair = std::make_pair(update[i + 1], update[i]);
                if (rules.count(pair)) {
                    correct = false;
                    break;
                }
            }
            // TODO
		}

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
