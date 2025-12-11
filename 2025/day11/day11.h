#pragma once

#include <map>
#include <string>
#include <vector>

namespace day11 {
    struct Device {
        std::string name;
        std::vector<std::string> outputs;
        int paths;
        int pending_inputs;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::map <std::string, Device> parse(const std::vector<std::string>& rows);
}
