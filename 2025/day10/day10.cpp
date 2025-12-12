#include "day10.h"
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
        auto& systems = parse_systems(machines);

        for (const auto& system : systems) {
            dump(system);
        }
        
        return -1;
    }

    std::ostream& operator<<(std::ostream& os, const Equation& equation) {
        for (const auto& coefficient : equation.coefficients) {
            os << std::setw(4) << coefficient;
        }
        os << std::setw(4) << "|" << std::setw(4) << equation.value;
        return os;
    }

    std::ostream& operator<<(std::ostream& os, const System& system) {
        for (const auto& upper_bound : system.upper_bounds) {
            os << std::setw(4) << upper_bound;
        }
        std::cout << std::endl;
        for (const auto& equation : system.equations) {
            std::cout << equation << std::endl;
        }
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

    std::vector<System> parse_systems(const std::vector<Machine>& machines) {
        std::vector<System> systems{};
        systems.reserve(machines.size());

        for (const auto& machine : machines) {
            auto& equations = parse_equations(machine);
            std::vector<int> upper_bounds{};
            upper_bounds.reserve(machine.buttons.size());
            for (size_t j = 0; j < machine.buttons.size(); j++) {
                int max = -1;
                for (size_t i = 0; i < machine.lamps; i++) {
                    const auto& equation = equations[i];
                    if (equation.coefficients[j] && (max == -1 || max > equation.value)) {
                        max = equation.value;
                    }
                }
                upper_bounds.push_back(max);
            }
            systems.push_back({upper_bounds, equations});
        }

        return systems;
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

    void dump(const System& system) {
        std::cout << system << std::endl;
        std::cout << std::endl;
    }
}
