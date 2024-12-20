#include <cmath>
#include <queue>
#include <set>
#include <utility>

#include "day18.h"

namespace day18 {
    using queue_t = std::priority_queue<std::pair<int, state>, std::vector<std::pair<int, state>>, std::greater<>>; 

    int part1(const std::vector<std::string>& rows, int size, int nanoseconds) {
        std::vector<std::vector<int>> grid;
        for (int row = 0; row <= size; row++) {
            grid.push_back(std::vector<int>(size + 1));
        }

        for (int i = 0; i < nanoseconds; i++) {
            const auto& coords = advent::ints(rows[i]);
            grid[coords[1]][coords[0]] = i + 1;
        }

        advent::coord start = {0,0};
        advent::coord end = {size, size};
        queue_t queue;
        state start_state = {0, start};
        queue.push(std::make_pair(heuristic(start_state, size), start_state));
        std::set<advent::coord> seen{start};
        auto min_cost = (size + 1) * (size + 1);

        while (!queue.empty() && queue.top().first < min_cost) {
            const auto current_state = queue.top().second;
            queue.pop();
            for (const auto& direction : advent::direction::nsew()) {
                const auto& new_position = current_state.position + direction;
                if (new_position.row < 0 || new_position.row > size ||
                    new_position.column < 0 || new_position.column > size) {
                    continue;
                }

                if (grid[new_position.row][new_position.column]) {
                    continue;
                }

                if (seen.count(new_position)) {
                    continue;
                }

                seen.insert(new_position);
                state new_state = {current_state.cost + 1, new_position};

                if (new_state.position == end) {
                    min_cost = std::min(min_cost, new_state.cost);
                    continue;
                }

                queue.push(std::make_pair(heuristic(new_state, size), new_state));
            }
        }

        return min_cost;
    }

    std::string part2(const std::vector<std::string>& rows, int size) {
        auto min{0};
        auto max{static_cast<int>(rows.size()) - 1};
        auto not_found{(size + 1) * (size + 1)};

        while (min < max - 1) {
            auto mid{(min + max) / 2};
            if (part1(rows, size, mid) == not_found) {
                max = mid;
            }
            else {
                min = mid;
            }
        }

        return rows[min];
    }

    int heuristic(const state& s, int size) {
        int manhattan = 2 * size - s.position.row - s.position.column;
        return s.cost + manhattan;
    }
}
