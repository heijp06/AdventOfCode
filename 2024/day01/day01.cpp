#include <algorithm>
#include <cmath>
#include <iterator>
#include <map>

#include "day01.h"
#include "../../lib/advent.h"

namespace day01 {
    int part1(const std::vector<std::string>& rows) {
        const auto& pairs = get_pairs(rows);
        const auto& list0 = get_list(0, pairs);
        const auto& list1 = get_list(1, pairs);

        auto sum{0};
        for (size_t i = 0; i < list1.size(); i++) {
            sum += std::abs(list0[i] - list1[i]);
        }

        return sum;
    }

    int part2(const std::vector<std::string>& rows) {
        const auto& pairs = get_pairs(rows);

        std::map<int, int> counts;
        for (const auto& pair : pairs) {
            int id = pair[1];
            counts[id]++;
        }

        auto sum{0};
        for (const auto& pair : pairs) {
            int id = pair[0];
            sum += id * counts[id];
        }

        return sum;
    }

    std::vector<std::vector<int>> get_pairs(const std::vector<std::string>& rows) {
        std::vector<std::vector<int>> pairs;
        std::transform(rows.cbegin(), rows.cend(), std::back_inserter(pairs), advent::ints<int>);

        return pairs;
    }

    std::vector<int> get_list(int index, const std::vector<std::vector<int>> ids) {
        std::vector<int> list;

        for (const auto& pair : ids) {
            list.push_back(pair.at(index));
        }

        std::sort(list.begin(), list.end());

        return list;
    }
}
