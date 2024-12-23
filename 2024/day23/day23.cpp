#include <algorithm>
#include <unordered_map>

#include "day23.h"
#include "../../lib/advent.h"

namespace day23 {
    int part1(const std::vector<std::string>& rows) {
        const auto& triples = get_triples(rows);

        auto counter{0};
        for (const auto& triple : triples) {
            for (const auto& value : triple) {
                if (value[0] == 't') {
                    counter++;
                    break;
                }
            }
        }

        return counter;
    }

     std::vector<std::vector<std::string>> get_triples(const std::vector<std::string>& rows) {
        std::unordered_map<std::string, std::vector<std::string>> pairs;

        for (const auto& row : rows) {
            const auto& pair = advent::split(row, "-");
            if (pair[0] < pair[1]) {
                pairs[pair[0]].push_back(pair[1]);
            }
            else {
                pairs[pair[1]].push_back(pair[0]);
            }
        }

        std::vector<std::vector<std::string>> triples;

        for (const auto& [value1, values1] : pairs) {
            for (const auto& value2 : values1) {
                if (pairs.count(value2)) {
                    const auto& values2 = pairs[value2];
                    for (const auto& value3 : values2) {
                        const auto& it = std::find(values1.cbegin(), values1.cend(), value3);
                        if (it != values1.cend()) {
                            triples.push_back({value1, value2, value3});
                        }
                    }
                }
            }
        }

        return triples;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
