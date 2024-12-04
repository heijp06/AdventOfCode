#pragma once

#include <string>
#include <vector>

namespace day04 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    bool check_char(const std::vector<std::string>& rows, char c, int row, int column);

    class xmas_counter {
    public:
        xmas_counter();
        void add_char(const char c);
        void new_line();
        int get_count();
    private:
        std::string current_;
        int count_;
    };
}
