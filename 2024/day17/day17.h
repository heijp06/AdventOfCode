#pragma once

#include <string>
#include <vector>

namespace day17 {
    std::string part1(const std::vector<std::string>& rows);
    std::string part2(const std::vector<std::string>& rows);

    class computer {
    public:
        computer(int a, int b, int c, const std::vector<int>& program);
        int a() const;
        int b() const;
        int c() const;
        std::string run();
    private:
        int a_;
        int b_;
        int c_;
        const std::vector<int> program_;
        int instruction_pointer_;
        std::string output_;
        int combo();
    };

    int combo(const computer& comp, int op);
}
