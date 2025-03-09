#include "day04.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::string start_at_midnight(const std::string& line) {
        if (line.at(12) != '2') {
            return line;
        }

        auto ones_digit = line.at(10);

        if (ones_digit == '9') {
            auto tens_digit = line.at(9);
            return line.substr(0, 9) + std::string(1, tens_digit + 1) + std::string("0 00:00") + line.substr(17);
        }

        return line.substr(0, 10) + std::string(1, ones_digit + 1) + std::string(" 00:00") + line.substr(17);
    }
}
