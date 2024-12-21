#pragma once

#include <string>
#include <vector>

namespace day21 {
    class numeric_keypad {
    public:
        numeric_keypad();
        std::string next(const char c);
    private:
        char current_;
        int row(const char c) const;
        int column(const char c) const;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    int cost(const std::string& code);
}
