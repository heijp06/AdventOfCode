#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "../../lib/advent.h"

namespace day09 {
    enum class EndType { Top, Middle, Bottom };

    struct TopOfRectangle {
        std::int64_t row;
        std::int64_t left;
        std::int64_t right;
        EndType left_type;
        EndType right_type;
    };

    std::int64_t part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    std::vector<TopOfRectangle> get_tops(std::vector<std::pair<int64_t, int64_t>>& pairs);
    std::vector<std::pair<int64_t, int64_t>> parse(const std::vector<std::string>& rows);
}
