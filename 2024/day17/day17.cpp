#include "day17.h"
#include "../../lib/advent.h"

namespace day17 {
    constexpr std::int64_t adv = 0;
    constexpr std::int64_t bxl = 1;
    constexpr std::int64_t bst = 2;
    constexpr std::int64_t jnz = 3;
    constexpr std::int64_t bxc = 4;
    constexpr std::int64_t out = 5;
    constexpr std::int64_t bdv = 6;
    constexpr std::int64_t cdv = 7;

    std::string part1(const std::vector<std::string>& rows) {
        auto comp = parse(rows);

        return comp.run();
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    computer parse(const std::vector<std::string>& rows) {
        return computer {
            advent::ints<std::int64_t>(rows[0])[0],
            advent::ints<std::int64_t>(rows[1])[0],
            advent::ints<std::int64_t>(rows[2])[0],
            advent::ints<std::int64_t>(rows[4])
        };
    }

    computer::computer(std::int64_t a, std::int64_t b, std::int64_t c, const std::vector<std::int64_t>& program) :
        a_{a},
        b_{b},
        c_{c},
        program_{program},
        instruction_pointer_{0},
        output_{} {
    }

    std::int64_t computer::a() const {
        return a_;
    }

    std::int64_t computer::b() const {
        return b_;
    }

    std::int64_t computer::c() const {
        return c_;
    }

    std::string computer::run() {
        while (instruction_pointer_ < static_cast<std::int64_t>(program_.size())) {
            switch (program_[instruction_pointer_++]) {
            case adv:
                a_ >>= combo();
                break;
            case bxl:
                b_ ^= program_[instruction_pointer_++];
                break;
            case bst:
                b_ = combo() % 8;
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
                b_ = a_ >> combo();
                break;
            case cdv:
                c_ = a_ >> combo();
                break;
            }
        }

        return output_;
    }

    std::int64_t computer::combo() {
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

