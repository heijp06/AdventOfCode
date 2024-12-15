#include "day15.h"

namespace day15 {
    int part1(const std::vector<std::string>& rows) {
        auto it = empty_row(rows);

        auto& grid = advent::grid(std::vector<std::string>(rows.cbegin(), it));
        const auto& directions = parse_directions(std::vector<std::string>(it + 1, rows.cend()));

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<std::string>::const_iterator empty_row(const std::vector<std::string>& rows) {
        auto it = rows.cbegin();

        while (!it->empty()) {
            ++it;
        }

        return it;
    }

    std::vector<advent::direction> parse_directions(const std::vector<std::string>& rows) {
        std::vector<advent::direction> directions;

        for (const auto& row : rows) {
            for (const auto& c : row) {
                switch (c) {
                case '^':
                    directions.push_back(advent::direction::up());
                    break;
                case 'v':
                    directions.push_back(advent::direction::down());
                    break;
                case '<':
                    directions.push_back(advent::direction::left());
                    break;
                case '>':
                    directions.push_back(advent::direction::right());
                    break;
                }
            }
        }

        return directions;
    }
}
