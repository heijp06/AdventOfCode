#pragma once

#include <string>
#include <vector>

namespace day17 {
    std::string part1(const std::vector<std::string>& rows);
    std::string part2(const std::vector<std::string>& rows);

    class computer {
    public:
        computer(int a, const std::vector<int>& program);
    private:
        int a_;
        int b_;
        int c_;
        int instruction_pointer_;
        const std::vector<int> program_;
    };
}
