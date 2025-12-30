#pragma once

#include <string>
#include <vector>

namespace day08 {
    class Reader {
    public:
        Reader(const std::vector<int>& numbers);
        int read();

    private:
        const std::vector<int>& numbers_;
        int pos_;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
