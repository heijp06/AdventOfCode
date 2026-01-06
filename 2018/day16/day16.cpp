#include <iomanip>
#include <iostream>

#include "day16.h"
#include "../../lib/advent.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<Test> tests{};
        tests.reserve(rows.size());
        std::vector<std::vector<int>> program{};
        program.reserve(rows.size());

        parse(rows, tests, program);

        int result{};

        for (const auto& test : tests) {
            int counter{};

            for (const auto& function : Device::functions) {
                Device device{};
                device.registers = test.before;
                function(device, test.instruction[1], test.instruction[2], test.instruction[3]);
                if (device.registers == test.after) {
                    counter++;
                }
            }

            if (counter >= 3) {
                result++;
            }
        }

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
        std::vector<Test> tests{};
        tests.reserve(rows.size());
        std::vector<std::vector<int>> program{};
        program.reserve(rows.size());

        parse(rows, tests, program);

        std::vector<std::vector<bool>> matrix = std::vector<std::vector<bool>>(16, std::vector<bool>(16, true));

        Device device{};
        for (const auto& test : tests) {
            for (int i = 0; i < 16; i++) {
                device.registers = test.before;
                Device::functions[i](device, test.instruction[1], test.instruction[2], test.instruction[3]);
                if (device.registers != test.after) {
                    matrix[test.instruction[0]][i] = false;
                }
            }
        }

        auto counts = std::vector<int>(16);
        for (int i = 0; i < 16; i++) {
            for (const auto& b : matrix[i]) {
                if (b) {
                    counts[i]++;
                }
            }
        }
        auto jump_table = std::vector<int>(16, -1);

        for (int i = 0; i < 16; i++) {
            const auto& it1 = std::find(counts.cbegin(), counts.cend(), 1);
            int row = it1 - counts.cbegin();

            const auto& it2 = std::find(matrix[row].cbegin(), matrix[row].cend(), true);
            int column = it2 - matrix[row].cbegin();

            jump_table[row] = column;

            for (int r = 0; r < 16; r++) {
                if (matrix[r][column]) {
                    matrix[r][column] = false;
                    counts[r]--;
                }
            }
        }

        device.registers = {0, 0, 0, 0};
        for (const auto& instruction : program) {
            int index = jump_table[instruction[0]];
            Device::functions[index](device, instruction[1], instruction[2], instruction[3]);
        }

        return device.registers[0];
    }

    void parse(const std::vector<std::string>& rows, std::vector<Test>& tests, std::vector<std::vector<int>>& program) {
        bool parse_tests{ true };

        for (auto it = rows.cbegin(); it != rows.cend(); ++it) {
            if (it->empty()) {
                parse_tests = false;
                continue;
            }

            if (parse_tests) {
                tests.push_back({ advent::ints(*it++), advent::ints(*it++), advent::ints(*it++) });
            }
            else {
                program.push_back(advent::ints(*it));
            }
        }
    }

    Device::Device() : registers(4, 0) {}

    void Device::addr(int a, int b, int c) {
        registers[c] = registers[a] + registers[b];
    }

    void Device::addi(int a, int b, int c) {
        registers[c] = registers[a] + b;
    }

    void Device::mulr(int a, int b, int c) {
        registers[c] = registers[a] * registers[b];
    }

    void Device::muli(int a, int b, int c) {
        registers[c] = registers[a] * b;
    }

    void Device::banr(int a, int b, int c) {
        registers[c] = registers[a] & registers[b];
    }

    void Device::bani(int a, int b, int c) {
        registers[c] = registers[a] & b;
    }

    void Device::borr(int a, int b, int c) {
        registers[c] = registers[a] | registers[b];
    }

    void Device::bori(int a, int b, int c) {
        registers[c] = registers[a] | b;
    }

    void Device::setr(int a, int b, int c) {
        (void)b;
        registers[c] = registers[a];
    }

    void Device::seti(int a, int b, int c) {
        (void)b;
        registers[c] = a;
    }

    void Device::gtir(int a, int b, int c) {
        registers[c] = a > registers[b] ? 1 : 0;
    }

    void Device::gtri(int a, int b, int c) {
        registers[c] = registers[a] > b ? 1 : 0;
    }

    void Device::gtrr(int a, int b, int c) {
        registers[c] = registers[a] > registers[b] ? 1 : 0;
    }

    void Device::eqir(int a, int b, int c) {
        registers[c] = a == registers[b] ? 1 : 0;
    }

    void Device::eqri(int a, int b, int c) {
        registers[c] = registers[a] == b ? 1 : 0;
    }

    void Device::eqrr(int a, int b, int c) {
        registers[c] = registers[a] == registers[b] ? 1 : 0;
    }

    std::vector<std::function<void(Device&, int, int, int)>> Device::functions = {
        &Device::addr,
        &Device::addi,
        &Device::mulr,
        &Device::muli,
        &Device::banr,
        &Device::bani,
        &Device::borr,
        &Device::bori,
        &Device::setr,
        &Device::seti,
        &Device::gtir,
        &Device::gtri,
        &Device::gtrr,
        &Device::eqir,
        &Device::eqri,
        &Device::eqrr
    };
}
