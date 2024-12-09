#pragma once

#include <string>
#include <vector>

namespace day09 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<int> parse(const std::string& parse);

    class compactor {
    public:
        explicit compactor(const std::string& disk_map);
        int get_last_file_number() const;
        int get_file_length() const;
        int read();

    private:
        struct read_state {
            int index;
            int counter;
        };
        std::vector<int> layout_;
        read_state front_;
        read_state back_;
        bool read_front_;
        int index_;
    };
}
