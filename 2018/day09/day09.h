#pragma once

#include <memory>
#include <string>
#include <vector>

namespace day09 {
    struct Node {
        int value;
        std::shared_ptr<Node> previous;
        std::shared_ptr<Node> next;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
}
