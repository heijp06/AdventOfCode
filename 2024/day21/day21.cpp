#include <map>

#include "day21.h"

namespace day21 {
    int part1(const std::vector<std::string>& rows) {
        auto result{0};

        for (const auto& row : rows) {
            result += cost(row);
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int cost(const std::string& code) {
        for (const auto key : code) {

        }

        return -1;
    }

    numeric_keypad::numeric_keypad() :
        current_{'A'} {
    }

    std::string numeric_keypad::next(const char key) {
        std::string result;

        for (int r = row(current_); r < row(key); r++) {
            result += '^';
        }

        for (int c = column(current_); c < column(key); c++) {
            result += '>';
        }

        for (int r = row(current_); r > row(key); r--) {
            result += 'v';
        }

        for (int c = column(current_); c > column(key); c--) {
            result += '<';
        }

        result += 'A';
        current_ = key;

        return result;
    }

    int numeric_keypad::row(const char key) const {
        return key == 'A' ? 0 : (key - '0' + 2) / 3;
    }

    int numeric_keypad::column(const char key) const {
        switch (key) {
        case 'A':
            return 2;
        case '0':
            return 1;
        default:
            return (key - '0' - 1) % 3;
        }
    }
}
