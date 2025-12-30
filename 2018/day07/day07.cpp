#include <algorithm>
#include <iterator>
#include <set>

#include "day07.h"

namespace day07 {
    std::string part1(const std::vector<std::string>& rows) {
        auto& pairs = parse(rows);
        std::string result{};
        std::set<char> available{};
        std::set<char> to{};

        while (!pairs.empty()) {
            std::set<char> from{};
            to.clear();

            for (const auto& pair : pairs) {
                from.insert(pair.first);
                to.insert(pair.second);
            }

            std::vector<char> difference{};
            difference.reserve(26);
            std::set_difference(from.begin(), from.end(), to.begin(), to.end(), std::back_inserter(difference));

            available.insert(difference.cbegin(), difference.cend());

            const auto c = *(available.cbegin());
            available.erase(available.cbegin());
            pairs.erase(std::remove_if(pairs.begin(), pairs.end(), [&](auto& p) { return p.first == c; }), pairs.cend());

            result += c;
        }

        for (const auto& c : available) {
            result += c;
        }

        for (const auto& c : to) {
            result += c;
        }

        return result;
    }

    std::vector<std::pair<char, char>> parse(const std::vector<std::string>& rows) {
        std::vector<std::pair<char, char>> pairs{};
        pairs.reserve(rows.size());

        for (const auto& row : rows) {
            pairs.push_back({row[5], row[36]});
        }

        return pairs;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
