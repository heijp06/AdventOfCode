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
        computer comp = {advent::ints(rows[0])[0], advent::ints(rows[1])[0],advent::ints(rows[2])[0], advent::ints(rows[4])};

        return comp.run();
    }

    std::string part2(const std::vector<std::string>& rows) {
        (void)rows;
        return "?";
    }

    computer::computer(int a, int b, int c, const std::vector<int>& program) :
        a_{a},
        b_{b},
        c_{c},
        program_{program},
        instruction_pointer_{0},
        output_{} {
    }

    int computer::a() const {
        return a_;
    }

    int computer::b() const {
        return b_;
    }

    int computer::c() const {
        return c_;
    }

    std::string computer::run() {
        while (instruction_pointer_ < static_cast<int>(program_.size())) {
            switch (program_[instruction_pointer_++]) {
            case adv:
                a_ /= 1 << combo();
                break;
            case bxl:
                b_ ^= program_[instruction_pointer_++];
                break;
            case bst:
                b_ = program_[instruction_pointer_++] % 8;
                break;
            case jnz:
                if (a_) {
                    instruction_pointer_ = program_[instruction_pointer_];
                }
                else {
                    instruction_pointer_++;
                }
                break;
            case bxc:
                instruction_pointer_++;
                b_ ^= c_;
                break;
            case out:
                if (output_.size()) {
                    output_ += ",";
                }
                output_ += std::to_string(combo() % 8);
                break;
            case bdv:
                b_ = a_ / 1 << combo();
                break;
            case cdv:
                c_ = a_ / 1 << combo();
                break;
            }
        }

        return output_;
    }

    int computer::combo() {
        auto op = program_[instruction_pointer_++];
        switch (op) {
        case 4:
            return a_;
        case 5:
            return b_;
        case 6:
            return c_;
        default:
            return op;
        }
    }
}

