#include <cmath>
#include <functional>
#include <map>
#include <queue>
#include <utility>

#include "day16.h"
#include <iostream>

namespace day16 {
    using queue_t = std::priority_queue<std::pair<int, reindeer>, std::vector<std::pair<int, reindeer>>, std::greater<>>; 

    int part1(const std::vector<std::string>& rows) {
        const auto& grid = advent::grid(rows);
        const auto& start = grid.find('S');
        const auto& end = grid.find('E');

        auto min_cost{2001 * grid.get_height() * grid.get_width()};
        reindeer first = {start, advent::direction::right(), {start}};
        std::map<reindeer, int> costs { {first, 0}};
        std::map<int, std::vector<reindeer>> reindeer_per_cost;
        queue_t queue;
        queue.push(std::make_pair(costs[first] + calculate_heuristic(first, end), first));

        auto counter{0};
        size_t queue_size{0};

        while (!queue.empty() && queue.top().first <= min_cost) {
            auto current = queue.top().second;
            queue.pop();

            auto next_position = current.position + current.direction;
            if (next_position == end) {
                auto cost = costs[current] + 1;
                current.trail.push_back(end);
                reindeer_per_cost[cost].push_back(current);
                min_cost = std::min(min_cost, cost);
                continue;
            }

            if (grid[next_position] != '#') {
                reindeer next = { next_position, current.direction };
                if (!costs.count(next) || costs[next] > costs[current] + 1) {
                    next.trail = std::vector<advent::coord>(current.trail);
                    next.trail.push_back(next.position);
                    costs[next] = costs[current] + 1;
                    queue.push(std::make_pair(costs[next] + calculate_heuristic(next, end), next));
                }
            }

            if (grid[current.position + current.direction.turn_left()] != '#') {
                reindeer next = { current.position, current.direction.turn_left() };
                if (!costs.count(next) || costs[next] > costs[current] + 1000) {
                    next.trail = std::vector<advent::coord>(current.trail);
                    costs[next] = costs[current] + 1000;
                    queue.push(std::make_pair(costs[next] + calculate_heuristic(next, end), next));
                }
            }

            if (grid[current.position + current.direction.turn_right()] != '#') {
                reindeer next = { current.position, current.direction.turn_right() };
                if (!costs.count(next) || costs[next] > costs[current] + 1000) {
                    next.trail = std::vector<advent::coord>(current.trail);
                    costs[next] = costs[current] + 1000;
                    queue.push(std::make_pair(costs[next] + calculate_heuristic(next, end), next));
                }
            }

            counter++;
            queue_size = std::max(queue_size, queue.size());
        }

        std::cout << counter << ", " << queue_size << std::endl;
        
        return min_cost;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int calculate_heuristic(const reindeer& state, const advent::coord& end) {
        return turn_heuristic(state, end) + move_heuristic(state, end);
    }

    int turn_heuristic(const reindeer& state, const advent::coord& end) {
        if (state.position.row > end.row) {
            if (state.direction == advent::direction::down()) {
                return 2000;
            }
            if (state.position.column < end.column) {
                if (state.direction == advent::direction::left()) {
                    return 2000;
                }
                return 1000;
            }
            if (state.direction == advent::direction::up()) {
                return 0;
            }
            return 1000;
        }
        if (state.position.column < end.column) {
            if (state.direction == advent::direction::left()) {
                return 2000;
            }
            if (state.direction == advent::direction::right()) {
                return 0;
            }
            return 1000;
        }
        return 0;
    }

    int move_heuristic(const reindeer& state, const advent::coord& end) {
        return state.position.row - end.row + end.column - state.position.column;
    }
}
