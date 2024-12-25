#include "day25.h"

namespace day25 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<std::vector<int>> keys;
        std::vector<std::vector<int>> locks;

        for (size_t i = 0; i < rows.size(); i += 8) {
            if (rows[i][0] == '#') {
                std::vector<int> key;
                for (size_t column = 0; column < 5; column++) {
                    auto count{0};
                    for (size_t row = 1; row <= 5; row++) {
                        count += rows[i + row][column] == '#';
                    }
                    key.push_back(count);
                }
                keys.push_back(key);
            }
            else {
                std::vector<int> lock;
                for (size_t column = 0; column < 5; column++) {
                    auto count{0};
                    for (size_t row = 1; row <= 5; row++) {
                        count += rows[i + row][column] == '#';
                    }
                    lock.push_back(count);
                }
                locks.push_back(lock);
            }
        }

        auto result{0};

        for (const auto& key : keys) {
            for (const auto& lock : locks) {
                auto fits{true};
                for (size_t i = 0; i < 5 && fits; i++) {
                    if (key[i] + lock[i] > 5) {
                        fits = false;
                    }
                }
                result += fits;
            }
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
