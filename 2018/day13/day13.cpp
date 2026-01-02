#include "day13.h"

namespace day13 {
    static bool operator<(const Cart& left, const Cart& right) {
        if (left.position == right.position) return left.direction < right.direction;
        return left.position < right.position;
    }

    std::string part1(const std::vector<std::string>& rows) {
        advent::grid grid{rows};
        auto current = parse_carts(grid);

        return "";
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::set<Cart> parse_carts(advent::grid& grid) {
        std::set<Cart> carts{};
        std::vector<std::pair<std::string, advent::direction>> directions{
            {">-", advent::direction::right()},
            {"v|", advent::direction::down()},
            {"<-", advent::direction::left()},
            {"^|", advent::direction::up()}
        };

        for (const auto& direction : directions) {
            for (const auto& position : grid.find_all(direction.first[0])) {
                carts.insert({position, direction.second});
                grid[position] = direction.first[1];
            }
        }

        return carts;
    }
}
