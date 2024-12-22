#include <cstdint>

#include "day22.h"
#include "../../lib/advent.h"

namespace day22 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t result{0};

        for (const auto& row : rows) {
            auto number = advent::ints<std::int64_t>(row)[0];
            for (int i = 0; i < 2000; i++) {
                number = next(number);
            }
            result += number;
        }

        return result;
    }

    std::int64_t next(std::int64_t number) {
        number = ((64 * number) ^ number) % 16777216;
        number = ((number / 32) ^ number) % 16777216;
        number = ((2048 * number) ^ number) % 16777216;

        return number;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }
}
