#include "day16.h"
#include "../../lib/advent.h"

namespace day16 {
    int part1(const std::vector<std::string>& rows) {
        int result{};

        for (auto it = rows.cbegin(); it != rows.cend(); ++it) {
            if (it->empty()) {
                break;
            }

            const auto& before = advent::ints(*it++);
            const auto& instruction = advent::ints(*it++);
            const auto& after = advent::ints(*it++);
            int counter{};

            for (const auto& pair : Device::functions) {
                Device device{};
                device.registers = before;
                pair.second(device, instruction[1], instruction[2], instruction[3]);
                if (device.registers == after) {
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
        (void)rows;
        return -1;
    }

    Device::Device() : registers(4, 0) {
    }

    void Device::execute(const std::string& opcode, int a, int b, int c) {
        functions[opcode](*this, a, b, c);
    }

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

    std::unordered_map<std::string, std::function<void(Device&, int, int, int)>> Device::functions = {
        {"addr", &Device::addr},
        {"addi", &Device::addi},
        {"mulr", &Device::mulr},
        {"muli", &Device::muli},
        {"banr", &Device::banr},
        {"bani", &Device::bani},
        {"borr", &Device::borr},
        {"bori", &Device::bori},
        {"setr", &Device::setr},
        {"seti", &Device::seti},
        {"gtir", &Device::gtir},
        {"gtri", &Device::gtri},
        {"gtrr", &Device::gtrr},
        {"eqir", &Device::eqir},
        {"eqri", &Device::eqri},
        {"eqrr", &Device::eqrr}
    };
}
