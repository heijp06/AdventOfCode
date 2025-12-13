#include "day10.h"
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <utility>

#include "../../lib/advent.h"

namespace day10 {
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
        for (const auto& equation : system.equations) {
            std::cout << std::endl << equation;
        }
        return os;
    }

    std::ostream& operator<<(std::ostream& os, const Solution& solution) {
        for (const auto& value : solution.values) {
            os << std::setw(4) << value;
        }
        return os;
    }


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
        int total{};
        const auto& machines = parse(rows);
        auto& systems = parse_systems(machines);

        for (const auto& system : systems) {
            //dump(system);
            int min = -1;
            for (const auto& solution : solve(system)) {
                if (!check(system, solution)) {
                    throw std::logic_error("Illegal solution returned.");
                }
                int sum = std::reduce(solution.values.cbegin(), solution.values.cend());
                if (min < 0 || min > sum) {
                    min = sum;
                }
                //std::cout << solution << std::endl;
            }
            //std::cout << min << std::endl << std::endl;
            total += min;
        }
        
        return total;
    }

    bool check(const System& system, const Solution& solution) {
        for (const auto& equation : system.equations) {
            int sum{};
            for (int i = 0; i < solution.values.size(); i++) {
                sum += equation.coefficients[i] * solution.values[i];
            }
            if (sum != equation.value) {
                std::cout << "ERR: " << solution << std::endl;
                return false;
            }
        }
        return true;
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

    std::vector<Solution> solve(const System& system) {
        //dump(system);
        std::vector<Solution> solutions;
        if (system.upper_bounds.empty()) {
            solutions.emplace_back(Solution{{}});
            return solutions;
        }
        const auto& upper_bound = system.upper_bounds.front();
        if (system.equations.empty()) {
            auto sub_system = System{
                {system.upper_bounds.cbegin() + 1, system.upper_bounds.cend()},
                {}
            };
            for (const auto& solution : solve(sub_system)) {
                for (int value = 0; value <= upper_bound; value++) {
                    std::vector<int> values{value};
                    values.reserve(system.upper_bounds.size());
                    values.insert(values.end(), solution.values.cbegin(), solution.values.cend());
                    solutions.emplace_back(Solution{values});
                }
            }
            return solutions;
        }

        if (system.equations.size() == 1 || system.upper_bounds.size() == 1) {
            const auto& equation = system.equations.front();
            if (system.upper_bounds.size() == 1) {
                auto coefficient = equation.coefficients.front();
                if (coefficient) {
                    int value = equation.value / coefficient;
                    if (value >= 0 && value <= upper_bound &&
                        value * equation.coefficients.front() == equation.value) {
                        solutions.emplace_back(Solution{{value}});
                    }
                }
                else if (!equation.value) {
                    for (int value = 0; value <= upper_bound; value++) {
                        solutions.emplace_back(Solution{{value}});
                    }
                }
                return solutions;
            }

            std::vector<int> upper_bounds{system.upper_bounds.cbegin() + 1, system.upper_bounds.end()};
            std::vector<int> coefficients = {equation.coefficients.cbegin() + 1, equation.coefficients.cend()};
            int coefficient = equation.coefficients.front();
            for (int i = 0; i <= upper_bound; i++) {
                std::vector<Equation> equations = {Equation{coefficients, equation.value - coefficient * i}};
                for (auto solution : solve({upper_bounds, equations})) {
                    solution.values.emplace(solution.values.begin(), i);
                    solutions.emplace_back(solution);
                }
            }
            return solutions;
        }

        int row_index = -1;
        for (int i = 0; i < system.equations.size(); i++) {
            if (system.equations[i].coefficients[0] != 0) {
                row_index = i;
                break;
            }
        }

        if (row_index == -1) {
            std::vector<Equation> sub_equations{};
            sub_equations.reserve(system.equations.size());
            for (const auto& equation : system.equations) {
                const Equation& sub_equation = {
                    {equation.coefficients.cbegin() + 1, equation.coefficients.cend()},
                    equation.value
                };
                sub_equations.emplace_back(sub_equation);
            }
            const System& sub_system = {
                {system.upper_bounds.cbegin() + 1, system.upper_bounds.cend()},
                sub_equations
            };
            for (const auto& sub_solution : solve(sub_system)) {
                for (int value = 0; value <= upper_bound; value++) {
                    std::vector<int> solution{value};
                    solution.reserve(system.upper_bounds.size());
                    solution.insert(solution.end(), sub_solution.values.cbegin(), sub_solution.values.cend());
                    solutions.push_back({solution});
                }
            }
        }
        else {
            const auto& sub_system = reduce(system, row_index);
            const auto& equation = system.equations[row_index];
            for (const auto& sub_solution : solve(sub_system)) {
                int sum{};
                for (int i = 0; i < sub_solution.values.size(); i++) {
                    sum += equation.coefficients[i + 1] * sub_solution.values[i];
                }
                int value = (equation.value - sum) / equation.coefficients[0];
                if (value >= 0 && value <= upper_bound &&
                    value * equation.coefficients[0] == equation.value - sum) {
                    std::vector<int> solution{value};
                    solution.reserve(system.upper_bounds.size());
                    solution.insert(solution.end(), sub_solution.values.cbegin(), sub_solution.values.cend());
                    solutions.push_back({solution});
                }
            }
        }

        return solutions;
    }

    System reduce(const System& system, const int row_index) {
        std::vector<Equation> equations{};
        equations.reserve(system.equations.size() - 1);
        const auto& remove = system.equations[row_index];
        for (int i = 0; i < system.equations.size(); i++) {
            if (i == row_index) {
                continue;
            }

            const auto& equation = system.equations[i];
            int gcd = std::gcd(remove.coefficients[0], equation.coefficients[0]);
            int factor_remove = equation.coefficients[0] / gcd;
            int factor_equation = remove.coefficients[0] / gcd;
            std::vector<int> coefficients{};
            coefficients.reserve(equation.coefficients.size() - 1);
            bool all_zero = true;
            for (int j = 1; j < equation.coefficients.size(); j++) {
                auto coefficient = factor_equation * equation.coefficients[j] - factor_remove * remove.coefficients[j];
                coefficients.push_back(coefficient);
                if (coefficient) {
                    all_zero = false;
                }
            }
            if (!all_zero) {
                equations.push_back({coefficients, factor_equation * equation.value - factor_remove * remove.value});
            }
        }

        auto upper_bounds = std::vector<int>(system.upper_bounds.cbegin() + 1, system.upper_bounds.cend());

        return System{upper_bounds, equations};
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
        std::cout << system << std::endl << std::endl;
    }
}
