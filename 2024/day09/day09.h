#pragma once

#include <string>
#include <vector>

namespace day09 {
    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::vector<int> parse(std::string parse);

    class compactor {
    public:
        compactor(const std::string& disk_map);
        int get_last_file_number() const;

    private:
        const std::vector<int> layout_;
    };
}
