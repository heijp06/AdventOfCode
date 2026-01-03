#pragma once

#include <functional>
#include <string>
#include <vector>

namespace day16 {
    struct Device {
        Device();
        void addr(int a, int b, int c);
        void addi(int a, int b, int c);
        void mulr(int a, int b, int c);
        void muli(int a, int b, int c);
        void banr(int a, int b, int c);
        void bani(int a, int b, int c);
        void borr(int a, int b, int c);
        void bori(int a, int b, int c);
        void setr(int a, int b, int c);
        void seti(int a, int b, int c);
        void gtir(int a, int b, int c);
        void gtri(int a, int b, int c);
        void gtrr(int a, int b, int c);
        void eqir(int a, int b, int c);
        void eqri(int a, int b, int c);
        void eqrr(int a, int b, int c);
        std::vector<int> registers;

        static std::vector<std::function<void(Device&, int, int, int)>> functions;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
