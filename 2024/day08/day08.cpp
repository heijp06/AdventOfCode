#include "day08.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows) {
        const auto& grid = parse(rows);

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    grid parse(std::vector<std::string> rows) {
        int height = rows.size();
        int width = rows[0].size();
        std::map<char, std::vector<coord>> antennas;

        for (int row = 0; row < height; row++) {
            const auto& line = rows[row];
            for (int column = 0; column < width; column++) {
                auto antenna = line[column];
                if (antenna != '.') {
                    antennas[line[column]].push_back({row, column});
                }
            }
        }

        return grid({height, width}, antennas);
    }

    grid::grid(const coord& size, const antennas_t& antennas) :
        size_{size},
        antennas_{antennas} {
    }

    int grid::get_height() const {
        return size_.row;
    }

    int grid::get_width() const {
        return size_.column;
    }

    const antennas_t grid::get_antennas() const {
        return antennas_t();
    }
}
