#include <algorithm>
#include <cmath>
#include <iterator>

#include "day01.h"
#include "../../lib/advent.h"

namespace day01 {
    int part1(const std::vector<std::string>& rows) {
        auto sum{0};
        std::vector<std::vector<int>> pairs;

        std::transform(rows.cbegin(), rows.cend(), std::back_inserter(pairs), advent::ints);

        const auto& list0 = get_list(0, pairs);
        const auto& list1 = get_list(1, pairs);

        for (size_t i = 0; i < list1.size(); i++) {
			sum += std::abs(list0[i] - list1[i]);
        }

        return sum;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
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
