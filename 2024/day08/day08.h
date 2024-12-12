#pragma once

#include <map>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    using antennas_t = std::map<char, std::vector<advent::coord>>;

    class grid {
    public:
        grid(const advent::coord& size, const antennas_t& antennas);
        int get_height() const;
        int get_width() const;
        const antennas_t& get_antennas() const;
    private:
        const advent::coord size_;
        const antennas_t antennas_;
    };

    const grid parse(std::vector<std::string> rows);
}
