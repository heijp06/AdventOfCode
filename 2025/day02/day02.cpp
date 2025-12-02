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

        return sum;
    }

    long part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    long sum_invalid(const std::string& start, const std::string& end)
    {
        if (start.length() % 2 && end.length() % 2) {
            return 0;
        }

        const auto& length = start.length() % 2
            ? static_cast<long>(end.length() / 2)
            : static_cast<long>(start.length() / 2);
        const auto& pow = static_cast<long>(std::pow(10, length - 1));
        auto min = start.length() % 2 ? pow : std::stol(start.substr(0, length));
        auto max = end.length() % 2 ? pow * 10 - 1 : std::stol(end.substr(0, length));

        auto min_val = min * pow * 10 + min;
        if (min_val < std::stol(start)) {
            min_val += pow * 10 + 1;
            min++;
            if (min_val > std::stol(end)) {
                return 0;
            }
        }

        auto max_val = max * pow * 10 + max;
        if (max_val > std::stol(end)) {
            max_val -= pow * 10 + 1;
            max--;
            if (max_val < std::stol(start)) {
                return 0;
            }
        }

        return (max - min + 1) * (max + min) / 2 * (pow * 10 + 1);
    }
}
