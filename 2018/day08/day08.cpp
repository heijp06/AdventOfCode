#include "day08.h"
#include "../../lib/advent.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows) {
        Reader reader{advent::ints(rows[0])};

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    Reader::Reader(const std::vector<int>& numbers) : numbers_{numbers}, pos_{0} {}

    int day08::Reader::read() {
        return numbers_[pos_++];
    }
}
