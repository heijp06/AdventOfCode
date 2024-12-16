#include "day16.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows) {
        const auto& maze = advent::grid(rows);

        const auto& start = maze.find('S');
        const auto& end = maze.find('E');

        const reindeer first = {0, start, advent::direction::right()};

        // Map of positions seen.
        // Cost = 1000 * width * height;
        // using queue_t = std::priority_queue<int, std::vector<int>, std::greater<>>; 
        // #include <queue>

        
        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
