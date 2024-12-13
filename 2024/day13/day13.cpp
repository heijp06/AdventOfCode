#include "day13.h"
#include "../../lib/advent.h"

namespace day13 {
    int part1(const std::vector<std::string>& rows) {
        auto tokens{0};

        for (size_t i = 0; i < rows.size(); i += 4) {
            auto data = advent::ints(rows[i]);
            advent::coord a = { data[1], data[0] };

            data = advent::ints(rows[i + 1]);
            advent::coord b = { data[1], data[0] };

            data = advent::ints(rows[i + 2]);
            advent::coord prize = { data[1], data[0] };

            auto min_cost{ 0 };
            for (int i = 0; i <= 100; i++) {
                for (int j = 0; j <= 100; j++) {
                    if (i * a.row + j * b.row == prize.row &&
                        i * a.column + j * b.column == prize.column) {
                        auto cost = 3 * i + j;
                        if (!min_cost || cost < min_cost) {
                            min_cost = cost;
                        }
                    }
                }
            }

            tokens += min_cost;
        }

        return tokens;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
