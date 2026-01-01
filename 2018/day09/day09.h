#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace day09 {
    struct Node {
        std::int64_t value;
        std::shared_ptr<Node> previous;
        std::shared_ptr<Node> next;
    };

    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::int64_t solve(const std::vector<std::string>& rows, std::int64_t multiplier);
}
