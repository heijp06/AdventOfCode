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
        auto output = comp.run();
        std::string result{};

        for (const auto& value : output) {
            if (!result.empty()) {
                result += ",";
            }
            result += std::to_string(value);
        }

        return result;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        const auto program = parse(rows).program();

        data_t quines{{0}};
        for (int i = program.size() - 1; i >= 0; i--) {
            data_t new_quines;
            for (const auto& quine : quines) {
                for (std::int64_t j = 0; j < 8; j++) {
                    auto new_quine = 8 * quine + j;
                    auto comp = computer(new_quine, 0, 0, program);
                    if (comp.run()[0] == program[i]) {
                        new_quines.push_back(new_quine);
                    }
                }
            }
            quines = new_quines;
        }

        return quines[0];
    }

    computer parse(const std::vector<std::string>& rows) {
        return computer {
            advent::ints<std::int64_t>(rows[0])[0],
            advent::ints<std::int64_t>(rows[1])[0],
            advent::ints<std::int64_t>(rows[2])[0],
            advent::ints<std::int64_t>(rows[4])
        };
    }

    computer::computer(std::int64_t a, std::int64_t b, std::int64_t c, const data_t& program) :
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

    data_t computer::run() {
        while (instruction_pointer_ < static_cast<std::int64_t>(program_.size())) {
            switch (program_[instruction_pointer_++]) {
            case adv:
                a_ >>= combo();
                break;
            case bxl:
                b_ ^= program_[instruction_pointer_++];
                break;
            case bst:
                b_ = combo() & 7;
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
                output_.push_back(combo() & 7);
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

    const data_t computer::program() const {
        return program_;
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

