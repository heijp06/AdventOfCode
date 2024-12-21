#include <map>

#include "day21.h"
#include "../../lib/advent.h"

namespace day21 {
    int part1(const std::vector<std::string>& rows) {
        auto result{0};

        for (const auto& row : rows) {
            result += cost(row) * advent::ints(row)[0];
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int cost(const std::string& code) {
        numeric_keypad numpad;
        directional_keypad dirpad1;
        directional_keypad dirpad2;
        std::string result;

        for (const char numkey : code) {
            for (const char dirkey1 : numpad.next(numkey)) {
                for (const char dirkey2 : dirpad1.next(dirkey1)) {
                    for (const char dirkey3 : dirpad2.next(dirkey2)) {
                        result += dirkey3;
                    }
                }
            }
        }

        return static_cast<int>(result.size());
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

    directional_keypad::directional_keypad() :
        current_{'A'} {
    }

    std::string directional_keypad::next(const char key) {
        std::string result;

        for (int r = row(current_); r > row(key); r--) {
            result += 'v';
        }

        for (int c = column(current_); c < column(key); c++) {
            result += '>';
        }

        for (int r = row(current_); r < row(key); r++) {
            result += '^';
        }

        for (int c = column(current_); c > column(key); c--) {
            result += '<';
        }

        result += 'A';
        current_ = key;

        return result;
    }

    int directional_keypad::row(const char key) const {
        return key == '^' || key == 'A';
    }

    int directional_keypad::column(const char key) const {
        switch (key) {
        case '<':
            return 0;
        case '^':
        case 'v':
            return 1;
        default:
            return 2;
        }
    }
}
