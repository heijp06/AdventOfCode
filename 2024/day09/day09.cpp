#include "day09.h"

namespace day09 {
    int part1(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::vector<int> parse(std::string parse) {
        std::vector<int> result;

        for (const auto c : parse) {
            result.push_back(c - '0');
        }

        return result;
    }

    compactor::compactor(const std::string& disk_map) :
        layout_{parse(disk_map)} {
    }

    int compactor::get_last_file_number() const {
        return static_cast<int>(layout_.size() / 2);
    }
}
