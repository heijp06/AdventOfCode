#include <cmath>

#include "../../lib/advent.h"
#include "day02.h"

namespace day02 {
    long part1(const std::vector<std::string>& rows) {
        long sum{0};
        for (const auto& pair : advent::split(rows[0], ",")) {
            const auto& fields = advent::split(pair, "-");
            sum += sum_invalid(fields[0], fields[1]);
        }

        return -1;
    }

    long part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    long sum_invalid(const std::string& start, const std::string& end)
    {
        const auto& length = static_cast<long>(start.length() / 2);

        const auto& min = start.length() % 2 ? static_cast<long>(std::pow(10, length)) : std::stol(start.substr(0, length));

        return min;
    }
}
