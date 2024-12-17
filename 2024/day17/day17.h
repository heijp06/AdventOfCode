#pragma once

#include <string>
#include <vector>

namespace day17 {
    std::string part1(const std::vector<std::string>& rows);
    std::string part2(const std::vector<std::string>& rows);

    struct computer {
        int a;
        int b;
        int c;
    };

    int combo(const computer& comp, int op);
}
