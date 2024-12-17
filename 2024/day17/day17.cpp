#include "day17.h"

namespace day17 {
    std::string part1(const std::vector<std::string>& rows) {
        (void)rows;
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
