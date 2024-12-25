#pragma once

#include <optional>
#include <set>

#include <string>
#include <unordered_map>
#include <vector>

namespace day24 {
    enum class operations {AND, OR, XOR};

    struct gate {
        std::string input1;
        std::string input2;
        std::string output;
        operations operation;

        friend bool operator==(const gate& left, const gate& right) {
            return left.input1 == right.input1
                && left.input2 == right.input1
                && left.output == right.output
                && left.operation == right.operation;
        }
    };

    class monitoring_device {
    public:
        explicit monitoring_device(const std::vector<gate>& gates);
        void set_wire(const std::string& wire, bool value);
        bool get_wire(const std::string& wire) const;
        void run();
        std::optional<bool> get_output(operations operation, std::optional<bool> input1, std::optional<bool> input2) const;
        std::optional<bool> get_input(std::unordered_map<std::string, bool> known_wires, std::string input) const;
    private:
        std::vector<gate> gates_;
        std::unordered_map<std::string, bool> known_wires_;
    };

    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::vector<gate> parse_gates(const std::vector<std::string>& rows);
    std::unordered_map<std::string, bool> parse_known_wires(const std::vector<std::string>& rows);
    std::set<std::string> get_z_wires(const std::vector<gate>& gates);
}
