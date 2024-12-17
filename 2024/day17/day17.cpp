#include "day17.h"
#include "../../lib/advent.h"

namespace day17 {
    constexpr int adv = 0;
    constexpr int bxl = 1;
    constexpr int bst = 2;
    constexpr int jnz = 3;
    constexpr int bxc = 4;
    constexpr int out = 5;
    constexpr int bdv = 6;
    constexpr int cdv = 7;

    std::string part1(const std::vector<std::string>& rows) {
        computer = { advent::ints(rows[0])[0], advent::ints(rows[1])[0],advent::ints(rows[2])[0] };
        const auto& program = advent::ints(rows[4]);
        int index = 0;

        while (index < static_cast<int>(program.size())) {
            switch (index) {
            case adv:

                break;
            case bxl:
                break;
            case bst:
                break;
            case jnz:
                break;
            case bxc:
                break;
            case out:
                break;
            case bdv:
                break;
            case cdv:
                break;
            }
        }

        std::string result{};

        return result;
    }

    std::string part2(const std::vector<std::string>& rows) {
        (void)rows;
        return "?";
    }

    int combo(const computer& comp, int op) {
        if (op < 4) {
            return op;
        }

        if (op == 4) {
            return comp.a;
        }

        if (op == 5) {
            return comp.b;
        }

        if (op == 6) {
            return comp.c;
        }

        throw std::domain_error("Illegal combo operand");
    }
}

