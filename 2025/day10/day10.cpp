#include "day10.h"
#include "day10.h"
#include <cmath>

#include "../../lib/advent.h"

namespace day10 {
    int part1(const std::vector<std::string>& rows) {
        const auto& machines = parse(rows);
        int sum{};
        int index{};

        for (const auto& machine : machines) {
            int fewest = machine.buttons.size();
            for (int presses = 0; presses < (1 << machine.buttons.size()); presses++) {
                int result{};
                int press = 1;
                int count{};
                for (size_t i = 0; i < machine.buttons.size(); i++) {
                    if (presses & press) {
                        result ^= machine.buttons[i];
                        count++;
                    }
                    press <<= 1;
                }
                if (machine.diagram == result) {
                    fewest = std::min(fewest, count);
                }
            }
            sum += fewest;
        }

        return sum;
    }

    int part2(const std::vector<std::string>& rows) {
        const auto& machines = parse(rows);
        
        for (const auto& machine : machines) {
            auto& equations = parse_equations(machine);
        }

        return -1;
    }

    std::ostream& operator<<(std::ostream& os, const Equation& arg) {
        for (const auto& coefficient : arg.coefficients) {
            os << std::setw(4) << coefficient;
        }
        os << std::setw(4) << "|" << std::setw(4) << arg.value;
        return os;
    }

    std::vector<Machine> day10::parse(const std::vector<std::string>& rows) {
        std::vector<Machine> machines;
        machines.reserve(rows.size());

        for (const auto& row : rows) {
            const auto& fields = advent::split(row, " ");

            int lamps = fields[0].size() - 2;

            int diagram{};
            int value = 1;
            for (size_t i = 1; i <= lamps; i++) {
                if (fields[0][i] == '#') {
                    diagram += value;
                }
                value <<= 1;
            }

            std::vector<int> buttons{};
            buttons.reserve(fields.size() - 2);
            for (size_t i = 1; i < fields.size() - 1; i++) {
                int wiring{};
                for (int wire : advent::ints(fields[i])) {
                    wiring += (1 << wire);
                }
                buttons.push_back(wiring);
            }

            machines.push_back({lamps, diagram, buttons, advent::ints(fields.back())});
        }

        return machines;
    }

    std::vector<Equation> parse_equations(const Machine& machine) {
        std::vector<Equation> equations{};
        equations.reserve(machine.lamps);

        int wire = 1;
        for (size_t i = 0; i < machine.lamps; i++) {
            std::vector<int> coefficients{};
            coefficients.reserve(machine.buttons.size());
            for (const auto& button : machine.buttons) {
                coefficients.push_back(button & wire ? 1 : 0);
            }
            equations.push_back({coefficients, machine.joltages[i]});
            wire <<= 1;
        }

        return equations;
    }

    void dump(const std::vector<Equation>& equations) {
        for (const auto& equation : equations) {
            std::cout << equation << std::endl;
        }
        std::cout << std::endl;
    }

    std::vector<int> upper_bounds(const std::vector<Equation>& equation) {
        return std::vector<int>();
    }
}
