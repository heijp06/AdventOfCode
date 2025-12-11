#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace day11 {
    struct Device {
        std::string name;
        std::vector<std::string> inputs;
        std::vector<std::string> outputs;
        std::int64_t paths;
        std::int64_t fft;
        std::int64_t dac;
        std::int64_t both;
        std::int64_t pending_inputs;
        bool reachable;
    };

    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::map <std::string, Device> parse(const std::vector<std::string>& rows, std::string start);
    void solve(const std::vector<std::string>& rows, std::map<std::string, day11::Device>& reactor, std::string start);
}
