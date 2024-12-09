#include "day09.h"

namespace day09 {
    int part1(const std::vector<std::string>& rows) {
        auto checksum{0};
        auto c = compactor{rows[0]};
        auto length = c.get_file_length();

        for (int i = 0; i < length; i++) {
            checksum += i * c.read();
        }

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
	    back_{static_cast<int>(layout_.size()) - 1, layout_.back()},
	    read_front_(true),
        index_(0) {
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

    int compactor::read()
    {
        return 9;
    }
}
