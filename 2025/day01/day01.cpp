#include "day01.h"

namespace day01 {
    int part1(const std::vector<std::string>& rows) {
        return get_password(rows);
    }

    int part2(const std::vector<std::string>& rows) {
        return get_password(rows, true);
    }

    int get_password(const std::vector<std::string>& rows, bool part2) {
        auto result{ 0 };
        auto sum{ 50 };

        for (const auto& row : rows) {
            auto number = std::stoi(row.substr(1));
            if (part2) {
                result += number / 100;
            }
            number %= 100;

            if (number == 0)
            {
                continue;
            }

            if (row[0] == 'L') {
                sum -= number;
                if (sum == 0) {
                    result++;
                }
                else if (sum < 0) {
                    if (sum != -number)
                    {
                        result += part2;
                    }
                    sum += 100;
                }
            }
            else {
                sum += number;
                if (sum >= 100) {
                    result += part2 || sum == 100;
                    sum -= 100;
                }
            }
        }

        return result;
    }

}
