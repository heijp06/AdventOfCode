#include "day08.h"
#include <iostream>
#include <map>
#include <memory>
#include <set>

#include "day08.h"
#include "../../lib/advent.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows, int times) {
        return solve(rows, times);
    }

    int part2(const std::vector<std::string>& rows) {
        return solve(rows, -1);
    }

    int solve(const std::vector<std::string>& rows, int times) {
        auto coords = std::vector<coord3d_t>();
        coords.reserve(rows.size());
        std::set<item_t> items;
        std::map <coord3d_t, std::shared_ptr<std::vector<coord3d_t>>> junctions;
        int counter{};

        for (const auto& row : rows) {
            const auto& fields = advent::ints(row);
            coords.emplace_back(coord3d_t{fields[0] , fields[1], fields[2]});
        }

        for (size_t i = 0; i < coords.size() - 1; ++i) {
            const auto& coord1 = coords[i];
            for (size_t j = i + 1; j < coords.size(); ++j) {
                const auto& coord2 = coords[j];
                std::int64_t distance =
                    (coord1.x - coord2.x) * (coord1.x - coord2.x) +
                    (coord1.y - coord2.y) * (coord1.y - coord2.y) +
                    (coord1.z - coord2.z) * (coord1.z - coord2.z);
                if (distance < 0) {
                    distance++;
                }
                const auto& item = item_t{distance, coord1, coord2};
                items.insert(item);
            }
        }

        for (const auto& item : items) {
            if (counter == times) {
                break;
            }
            counter++;
            if (junctions.count(item.p1)) {
                auto junction1 = junctions[item.p1];
                if (junctions.count(item.p2)) {
                    auto junction2 = junctions[item.p2];
                    if (junction1 == junction2) {
                        continue;
                    }
                    junction1->insert(junction1->end(), junction2->cbegin(), junction2->cend());
                    for (const auto& p : *junction2) {
                        junctions[p] = junction1;
                    }
                }
                else {
                    junction1->emplace_back(item.p2);
                    junctions[item.p2] = junction1;
                }
            }
            else if (junctions.count(item.p2)) {
                auto junction2 = junctions[item.p2];
                junction2->emplace_back(item.p1);
                junctions[item.p1] = junction2;
            }
            else {
                auto junction = std::make_shared<std::vector<coord3d_t>>(std::initializer_list{item.p1, item.p2});
                junctions[item.p1] = junction;
                junctions[item.p2] = junction;
            }

            if (times < 0 && rows.size() == junctions.cbegin()->second->size()) {
                return item.p1.x * item.p2.x;
            }
        }

        auto values = std::vector<std::shared_ptr<std::vector<coord3d_t>>>();
        values.reserve(junctions.size());

        for (auto it = junctions.cbegin(); it != junctions.cend(); it++) {
            values.emplace_back(it->second);
        }

        std::sort(values.begin(), values.end());
        auto it = std::unique(values.begin(), values.end());
        values.erase(it, values.end());

        auto sizes = std::vector<size_t>(values.size());
        std::transform(values.cbegin(), values.cend(), sizes.begin(), [](const auto& p) { return p->size(); });
        std::sort(sizes.rbegin(), sizes.rend());

        return sizes[0] * sizes[1] * sizes[2];
    }
}
