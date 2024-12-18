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
        computer comp = { advent::ints(rows[0])[0], advent::ints(rows[1])[0],advent::ints(rows[2])[0] };
        const auto& program = advent::ints(rows[4]);
        int index = 0;
        std::string result{};

        while (index < static_cast<int>(program.size())) {
            switch (program[index++]) {
            case adv:
                comp.a /= 1 << combo(comp, program[index++]);
                break;
            case bxl:
                comp.b ^= program[index++];
                break;
            case bst:
                comp.b = program[index++] % 8;
                break;
            case jnz:
                if (comp.a) {
                    index = program[index];
                }
                else {
                    index++;
                }
                break;
            case bxc:
                index++;
                comp.b ^= comp.c;
                break;
            case out:
                if (result.size()) {
                    result += ",";
                }
                result += std::to_string(combo(comp, program[index++]) % 8);
                break;
            case bdv:
                comp.b = comp.a / 1 << combo(comp, program[index++]);
                break;
            case cdv:
                comp.c = comp.a / 1 << combo(comp, program[index++]);
                break;
            }
        }

        return result;
    }

    std::string part2(const std::vector<std::string>& rows) {
        (void)rows;
        return "?";
    }

    int combo(const computer& comp, int op) {
        switch (op) {
        case 4:
            return comp.a;
        case 5:
            return comp.b;
        case 6:
            return comp.c;
        default:
            return op;
        }
    }
}

