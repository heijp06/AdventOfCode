#pragma once

#include <map>
#include <string>
#include <vector>

namespace day08 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    struct coord {
        int row;
        int column;
    };

    using antennas_t = std::map<char, std::vector<coord>>;

    class grid {
    public:
        grid(const coord& size, const antennas_t& antennas);
        int get_height() const;
        int get_width() const;
        const antennas_t get_antennas() const;
    private:
        const coord& size_;
        const antennas_t& antennas_;
    };

    grid parse(std::vector<std::string> rows);
}
