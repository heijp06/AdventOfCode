#pragma once

#include <string>
#include <vector>

namespace day09 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<int> parse(const std::string& parse);

    class compactor {
    public:
        compactor(const std::string& disk_map);
        int get_last_file_number() const;
        int get_file_length() const;

    private:
        std::vector<int> layout_;
    };
}
