#include <functional>
#include <map>
#include <queue>
#include <utility>

#include "day16.h"

namespace day16 {
    using queue_t = std::priority_queue<reindeer, std::vector<reindeer>, std::greater<>>; 

    int part1(const std::vector<std::string>& rows) {
        const auto& grid = advent::grid(rows);

        const auto& start = grid.find('S');
        const auto& end = grid.find('E');

        auto min_cost{0};
        reindeer first = {start, advent::direction::right()};
        std::map<reindeer, int> costs { {first, 0}};
        queue_t queue;
        queue.push(first);

        while (!queue.empty() && costs[queue.top()] < min_cost) {
            auto current = queue.top();
            queue.pop();

            auto next_position = current.position + current.direction;
            if (grid[next_position] != '#') {
                reindeer next = { next_position, current.direction };
                if (!costs.count(next) || costs[next] > costs[current] + 1) {
                    costs[next] = costs[current] + 1;
                    queue.push(next);
                }
            }

            //if (grid[current.position + current.direction.turn_left()] != '#') {
            //    reindeer step = { current.cost + 1000, current.position, current.direction.turn_left() };
            //    auto p = std::make_pair(step.position, step.direction);
            //    if (!costs.count(p) || costs[p] > step.cost) {
            //        costs[p] = step.cost;
            //        queue.push(step);
            //    }
            //}

            //if (grid[current.position + current.direction.turn_right()] != '#') {
            //    reindeer step = { current.cost + 1000, current.position, current.direction.turn_right() };
            //    auto p = std::make_pair(step.position, step.direction);
            //    if (!costs.count(p) || costs[p] > step.cost) {
            //        costs[p] = step.cost;
            //        queue.push(step);
            //    }
            //}
        }
        
        return min_cost;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
