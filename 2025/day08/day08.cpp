#include <set>

#include "day08.h"
#include "../../lib/advent.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows) {
        auto coords = std::vector<coord3d_t>();
        coords.reserve(rows.size());
        std::set<item_t> items;

        for (const auto& row : rows) {
            const auto& fields = advent::ints(row);
            coords.emplace_back(coord3d_t{ fields[0] , fields[1], fields[2] });
        }

        for (size_t i = 0; i < coords.size() - 1; ++i)
        {
            const auto& coord1 = coords[i];
            for (size_t j = i + 1; j < coords.size(); ++j)
            {
                const auto& coord2 = coords[j];
                std::int64_t distance =
                    (coord1.x - coord2.x) * (coord1.x - coord2.x) +
                    (coord1.y - coord2.y) * (coord1.y - coord2.y) +
                    (coord1.z - coord2.z) * (coord1.z - coord2.z);
                const auto& item = item_t{ distance, coord1, coord2 };
                items.insert(item);
            }
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
