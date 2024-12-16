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
        reindeer first = {0, start, advent::direction::right()};
        std::map<std::pair<advent::coord, advent::direction>, int> min_costs{ {std::make_pair(first.position, first.direction), 0}};
        queue_t queue;
        queue.push(first);

        while (!queue.empty() && queue.top().cost < min_cost) {
            auto next = queue.top();
            queue.pop();

            auto forwards = next.position + next.direction;
            if (grid[forwards] != '#') {
                reindeer step = { next.cost + 1, forwards, next.direction };
                auto p = std::make_pair(step.position, step.direction);
                if (!min_costs.count(p) || min_costs[p] > step.cost) {
                    min_costs[p] = step.cost;
                    queue.push(step);
                }
            }

            if (grid[next.position + next.direction.turn_left()] != '#') {
                reindeer step = { next.cost + 1000, next.position, next.direction.turn_left() };
                auto p = std::make_pair(step.position, step.direction);
                if (!min_costs.count(p) || min_costs[p] > step.cost) {
                    min_costs[p] = step.cost;
                    queue.push(step);
                }
            }

            if (grid[next.position + next.direction.turn_right()] != '#') {
                reindeer step = { next.cost + 1000, next.position, next.direction.turn_right() };
                auto p = std::make_pair(step.position, step.direction);
                if (!min_costs.count(p) || min_costs[p] > step.cost) {
                    min_costs[p] = step.cost;
                    queue.push(step);
                }
            }
        }
        
        return min_cost;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
