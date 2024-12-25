#include <cstdint>
#include <stdexcept>

#include "day24.h"
#include "../../lib/advent.h"

namespace day24 {
    std::int64_t part1(const std::vector<std::string>& rows) {
        auto& gates = parse_gates(rows);
        auto& known_wires = parse_known_wires(rows);
        const auto& z_wires = get_z_wires(gates);
        auto unknown_z_wires = z_wires;

        while (!unknown_z_wires.empty()) {
            std::vector<gate> new_gates;
            for (const auto& gate : gates) {
                const auto& input1 = get_input(known_wires, gate.input1);
                const auto& input2 = get_input(known_wires, gate.input2);
                const auto& output = get_output(gate.operation, input1, input2);
                if (!output.has_value()) {
                    new_gates.push_back(gate);
                    continue;
                }

                known_wires[gate.output] = output.value();

                if (gate.output[0] == 'z') {
                    unknown_z_wires.erase(gate.output);
                }
            }
            gates = new_gates;
        }

        std::int64_t result{0};
        std::int64_t bit_value{1};

        for (const auto& z_wire : z_wires) {
            if (known_wires[z_wire]){
                result += bit_value;
            }
            bit_value <<= 1;
        }

        return result;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<gate> parse_gates(const std::vector<std::string>& rows) {
        std::vector<gate> result;

        auto can_read{false};
        for (const auto& row : rows) {
            if (!can_read) {
                can_read = row.empty();
                continue;
            }

            const auto& fields = advent::split(row, " ");
            auto operation = fields[1] == "AND"
                ? operations::AND
                : fields[1] == "OR" ? operations::OR : operations::XOR;
            result.push_back({fields[0], fields[2], fields[4], operation});
        }

        return result;
    }

    std::unordered_map<std::string, bool> parse_known_wires(const std::vector<std::string>& rows) {
        std::unordered_map<std::string, bool> result;

        for (const auto& row : rows) {
            if (row.empty()) {
                break;
            }

            const auto& fields = advent::split(row, ": ");
            result[fields[0]] = fields[1] == "1";
        }

        return result;
    }

    std::set<std::string> get_z_wires(const std::vector<gate>& gates) {
        std::set<std::string> z_wires;

        for (const auto& gate : gates) {
            if (gate.output[0] == 'z') {
                z_wires.insert(gate.output);
            }
        }

        return z_wires;
    }

    std::optional<bool> get_output(operations operation, std::optional<bool> input1, std::optional<bool> input2) {
        switch (operation) {
        case day24::operations::AND:
            if (input1.has_value() && !input1.value()) {
                return false;
            }
            if (input2.has_value() && !input2.value()) {
                return false;
            }
            if (input1.has_value() && input2.has_value()) {
                return true;
            }
            return std::nullopt;
        case day24::operations::OR:
            if (input1.has_value() && input1.value()) {
                return true;
            }
            if (input2.has_value() && input2.value()) {
                return true;
            }
            if (input1.has_value() && input2.has_value()) {
                return false;
            }
            return std::nullopt;
        case day24::operations::XOR:
            if (input1.has_value() && input2.has_value()) {
                return input1.value() ^ input2.value();
            }
            return std::nullopt;
        default:
            throw std::domain_error("Unknown operation.");
        }
    }

    std::optional<bool> get_input(std::unordered_map<std::string, bool> known_wires, std::string input) {
        return known_wires.count(input) ? std::optional<bool>(known_wires[input]) : std::nullopt;
    }
}
