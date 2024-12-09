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

    std::vector<int> parse(const std::string& parse) {
        std::vector<int> result;

        for (const auto c : parse) {
            result.push_back(c - '0');
        }

        return result;
    }

    compactor::compactor(const std::string& disk_map) :
        layout_{parse(disk_map)},
        front_{0, layout_[0]},
        back_{static_cast<int>(layout_.size()), 0} {
    }

    int compactor::get_last_file_number() const {
        return static_cast<int>(layout_.size() / 2);
    }

    int compactor::get_file_length() const
    {
        auto length{0};

        for (size_t i = 0; i < layout_.size(); i += 2) {
            length += layout_[i];
        }

        return length;
    }
}
