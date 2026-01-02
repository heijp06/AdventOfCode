#include "day14.h"
#include "../../lib/advent.h"

namespace day14 {
    std::string part1(const std::vector<std::string>& rows) {
        const auto number = advent::ints(rows.front()).front();
        std::vector<int> scores{};
        scores.reserve(number + 11);

        scores.push_back(3);
        scores.push_back(7);

        size_t elve1{0};
        size_t elve2{1};

        while (scores.size() < number + 10) {
            int sum = scores[elve1] + scores[elve2];
            int d1 = sum / 10;
            int d2 = sum % 10;
            if (d1) {
                scores.push_back(d1);
            }
            scores.push_back(d2);
            elve1 = (elve1 + scores[elve1] + 1) % scores.size();
            elve2 = (elve2 + scores[elve2] + 1) % scores.size();
        }

        std::string result{};

        for (size_t i = 0; i < 10; i++) {
            result += std::to_string(scores[number + i]);
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
