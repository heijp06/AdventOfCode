#include "day03.h"
#include "../lib/advent.h"

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
        return claim(ints[0], ints[1], ints[2], ints[3], ints[4]);
    }

    claim::claim(int id, int left, int top, int width, int height) :
        id_{id}, left_{left}, top_{top}, width_{width}, height_{height} {
    }

    int claim::id() const {
        return id_;
    }

    int claim::left() const {
        return left_;
    }

    int claim::top() const {
        return top_;
    }

    int claim::width() const {
        return width_;
    }

    int claim::height() const {
        return height_;
    }
}
