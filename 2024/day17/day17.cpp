#include "day17.h"

#include <iostream>

namespace day17 {
    std::string part1(const std::vector<std::string>& rows) {
        std::string result{};

        int a = 46187030;
        int b = 0;
        int c = 0;

        while (a) {
            b = a % 8;
            b = b ^ 1;
            c = a / (1 << b);
            a = a / 8;
            b = b ^ c;
            b = b ^ 6;
            int x = b % 8;
            std::cout << x << ",";
        }

        return "?";
    }

    std::string part2(const std::vector<std::string>& rows) {
        (void)rows;
        return "?";
    }

    computer::computer(int a, const std::vector<int>& program) :
        a_{a},
        b_{0},
        c_{0},
        instruction_pointer_{0},
        program_{program} { 
    }
}
