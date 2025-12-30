#include <algorithm>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <set>

#include "day07.h"

namespace day07 {
    std::string part1(const std::vector<std::string>& rows) {
        return solve(rows, 1, 0).first;
    }

    int part2(const std::vector<std::string>& rows, int workers, int delay) {
        return solve(rows, workers, delay).second;
    }

    std::pair<std::string, int> solve(const std::vector<std::string>& rows, int number_of_workers, int delay) {
        const int width = 16;
        auto& pairs = parse(rows);
        std::string result{};
        std::set<char> available{};
        std::set<char> to{};
        std::set<char> working{};
        int time{};
        auto update{true};
        auto last{'.'};

        std::vector<Worker> workers{};
        workers.reserve(number_of_workers);
        for (size_t i = 0; i < number_of_workers; i++) {
            workers.push_back({'.', 0});
        }

        while (!pairs.empty()) {
            for (size_t i = 0; i < number_of_workers; i++) {
                auto& worker = workers[i];
                if (worker.timer) {
                    worker.timer--;
                    if (!worker.timer) {
                        pairs.erase(std::remove_if(pairs.begin(), pairs.end(), [&](auto& p) { return p.first == worker.step; }), pairs.cend());
                        result += worker.step;
                        worker.step = '.';
                        update = true;
                    }
                }
            }

            if (update) {
                update = false;
                std::set<char> from{};
                to.clear();

                for (const auto& pair : pairs) {
                    from.insert(pair.first);
                    to.insert(pair.second);
                    last = pair.second;
                }
                to.insert(working.cbegin(), working.cend());

                std::vector<char> difference{};
                difference.reserve(26);
                std::set_difference(from.begin(), from.end(), to.begin(), to.end(), std::back_inserter(difference));

                available.insert(difference.cbegin(), difference.cend());
            }

            for (size_t i = 0; !available.empty() && i < number_of_workers; i++) {
                auto& worker = workers[i];
                if (!worker.timer) {
                    const auto c = *(available.cbegin());
                    available.erase(available.cbegin());
                    worker.timer = delay + c - 'A' + 1;
                    worker.step = c;
                    working.insert(c);
                }
            }

            //std::cout << std::setw(width) << time;

            //for (size_t i = 0; i < number_of_workers; i++) {
            //    std::cout << std::setw(width) << workers[i].step;
            //}

            //std::cout << std::setw(width) << result << std::endl;

            time++;
        }

        result += last;
        time += delay + last - 'A';

        return {result, time};
    }

    std::vector<std::pair<char, char>> parse(const std::vector<std::string>& rows) {
        std::vector<std::pair<char, char>> pairs{};
        pairs.reserve(rows.size());

        for (const auto& row : rows) {
            pairs.push_back({row[5], row[36]});
        }

        return pairs;
    }
}
