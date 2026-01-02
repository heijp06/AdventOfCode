#include "day14.h"
#include "../../lib/advent.h"

namespace day14 {
    std::string part1(const std::vector<std::string>& rows) {
        return solve(rows, false).first;
    }

    size_t part2(const std::vector<std::string>& rows) {
        return solve(rows, true).second;
    }

    std::pair<std::string, size_t> solve(const std::vector<std::string>& rows, bool part2) {
        const auto& row = rows.front();
        const auto number = advent::ints(row).front();
        std::vector<int> scores{};
        scores.reserve(number + 11);

        scores.push_back(3);
        scores.push_back(7);

        size_t elve1{0};
        size_t elve2{1};

        while (scores.size() < number + 10 || part2) {
            int sum = scores[elve1] + scores[elve2];
            int d1 = sum / 10;
            int d2 = sum % 10;
            if (d1) {
                scores.push_back(d1);
                if (part2) {
                    auto index = check(row, scores);
                    if (index) {
                        return {"", index};
                    }
                }
            }
            scores.push_back(d2);
            if (part2) {
                auto index = check(row, scores);
                if (index) {
                    return {"", index};
                }
            }
            elve1 = (elve1 + scores[elve1] + 1) % scores.size();
            elve2 = (elve2 + scores[elve2] + 1) % scores.size();
        }

        std::string result{};

        for (size_t i = 0; i < 10; i++) {
            result += std::to_string(scores[number + i]);
        }

        return {result, 0};
    }

    size_t day14::check(const std::string& row, const std::vector<int>& scores) {
        if (row.size() > scores.size()) {
            return 0;
        }

        auto it_scores = scores.crbegin();
        for (auto it_row = row.crbegin(); it_row != row.crend(); it_row++, it_scores++) {
            if (*it_row - '0' != *it_scores) {
                return 0;
            }
        }

        return scores.size() - row.size();
    }
}
