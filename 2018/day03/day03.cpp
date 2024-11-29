#include "day03.h"
#include "../../lib/advent.h"

namespace day03 {
    int part1(const std::vector<std::string>& rows) {
        std::vector<claim> claims;

        for (const auto& row : rows) {
            claims.push_back(parse(row));
        }

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    claim parse(std::string row) {
        const auto& ints = advent::ints(row);
        return claim{ints[0], ints[1], ints[2], ints[3], ints[4]};
    }

    Leaf::Leaf(int size, int overlap) : size_{size}, overlap_{overlap} {
    }

    int Leaf::size() {
        return size_;
    }

    void Leaf::insert(claim claim) {
        (void)claim;
    }
}
