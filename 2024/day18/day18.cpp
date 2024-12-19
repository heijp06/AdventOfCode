#include "day18.h"
#include "../../lib/advent.h"

namespace day18 {
    int part1(const std::vector<std::string>& rows, int size, int nanoseconds) {
        std::vector<std::vector<int>> grid;
        for (int row = 0; row <= size; row++) {
            grid.push_back(std::vector<int>(size + 1));
        }

        for (int i = 0; i < nanoseconds; i++) {
            const auto& coords = advent::ints(rows[i]);
            grid[coords[1]][coords[0]] = i + 1;
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
