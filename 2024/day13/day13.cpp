#include <stdexcept>

#include "day13.h"

namespace day13 {
    int part1(const std::vector<std::string>& rows) {
        auto tokens{0};

        for (size_t i = 0; i < rows.size(); i += 4) {
            auto data = advent::ints(rows[i]);
            advent::coord a = { data[1], data[0] };

            data = advent::ints(rows[i + 1]);
            advent::coord b = { data[1], data[0] };

            data = advent::ints(rows[i + 2]);
            advent::coord prize = { data[1], data[0] };

            tokens += solve(a, b, prize);
        }

        return tokens;
    }

    int64_t part2(const std::vector<std::string>& rows) {
        int64_t tokens{0};

        for (size_t i = 0; i < rows.size(); i += 4) {
            auto data = advent::ints(rows[i]);
            advent::coord a = { data[1], data[0] };

            data = advent::ints(rows[i + 1]);
            advent::coord b = { data[1], data[0] };

            data = advent::ints(rows[i + 2]);
            advent::coord prize = { data[1], data[0] };

            tokens += solve(a, b, prize, true);
        }

        return tokens;
    }

    int64_t solve(const advent::coord& a, const advent::coord& b, const advent::coord& prize, bool part2) {
        int64_t to_add = part2 ? 10'000'000'000'000 : 0;
        int64_t factor = a.row * b.column - a.column * b.row;
        int64_t sum = a.row * (prize.column + to_add) - a.column * (prize.row + to_add);

        if (factor == 0) {
            throw std::domain_error("factor or sum is 0.");
        }

        if (sum % factor) {
            return 0;
        }

        int64_t b_presses = sum / factor;

        if (b_presses < 0) {
            return 0;
        }

        int64_t sum2 = (prize.column + to_add) - b_presses * b.column;

        if (sum2 % a.column) {
            return 0;
        }

        int64_t a_presses = sum2 / a.column;

        return 3 * a_presses + b_presses;
    }
}
