#include <cmath>
#include <unordered_map>

#include "day22.h"
#include "../../lib/advent.h"

namespace day22 {
    using table_t = std::unordered_map<Key, int>;

    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t result{0};

        for (const auto& row : rows) {
            auto number = advent::ints<std::int64_t>(row)[0];
            for (int i = 0; i < 2000; i++) {
                number = next(number);
            }
            result += number;
        }

        return result;
    }

    std::int64_t next(std::int64_t number) {
        number = ((64 * number) ^ number) % 16777216;
        number = ((number / 32) ^ number) % 16777216;
        number = ((2048 * number) ^ number) % 16777216;

        return number;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        std::vector<table_t> tables;

        for (const auto& row : rows) {
            std::vector<int> changes;
            std::vector<int> numbers;

            auto number = advent::ints<std::int64_t>(row)[0];
            for (int i = 0; i < 2000; i++) {
                int d0 = static_cast<int>(number % 10);
                number = next(number);
                int d1 = static_cast<int>(number % 10);
                changes.push_back(d1 - d0);
                numbers.push_back(d1);
            }

            table_t table;
            for (int i = 0; i < 1997; i++) {
                Key sequence = {changes[i], changes[i + 1], changes[i + 2], changes[i + 3]};
                if (table.count(sequence)) {
                    continue;
                }
                table[sequence] = numbers[i + 3];
            }
            tables.push_back(table);
        }

        auto max_bananas{0};

        for (int change1 = -9; change1 <= 9; change1++) {
            for (int change2 = -9; change2 <= 9; change2++) {
                for (int change3 = -9; change3 <= 9; change3++) {
                    for (int change4 = -9; change4 <= 9; change4++) {
                        const Key sequence = {change1, change2, change3, change4};
                        auto bananas{0};
                        for (const auto& table : tables) {
                            if (table.count(sequence)) {
                                bananas += table.at(sequence);
                            }
                        }
                        max_bananas = std::max(max_bananas, bananas);
                    }
                }
            }
        }

        return max_bananas;
    }
}
