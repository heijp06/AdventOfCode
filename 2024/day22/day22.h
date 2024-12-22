#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day22 {
    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::int64_t next(std::int64_t number);

    struct Key {
        int number1;
        int number2;
        int number3;
        int number4;

        friend bool operator==(const Key& left, const Key& other) {
            return left.number1 == other.number1 && left.number2 == other.number2
                && left.number3 == other.number3 && left.number4 == other.number4;
        }
    };
}

namespace std {
    template<>
    struct hash<day22::Key> {
        std::size_t operator()(const day22::Key& k) const {
            return 18 * 18 * 18 * (k.number1 + 9) + 18 * 18 * (k.number2 + 9)
                + 18 * (k.number3 + 9) + (k.number4 + 9);
        }
    };
}
