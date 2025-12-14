#include <algorithm>
#include <cmath>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    bool static operator==(const TopOfRectangle& left, const TopOfRectangle& right) {
        return
            left.row == right.row &&
            left.left == right.left &&
            left.right == right.right &&
            left.left_is_red == right.left_is_red &&
            left.right_is_red == right.right_is_red;
    }

    bool static operator!=(const TopOfRectangle& left, const TopOfRectangle& right) {
        return !(left == right);
    }

    bool static operator<(const TopOfRectangle& left, const TopOfRectangle& right) {
        if (left.row < right.row) return true;
        if (left.row > right.row) return false;
        if (left.left < right.left) return true;
        if (left.left > right.left) return false;
        if (left.right < right.right) return true;
        if (left.right > right.right) return false;
        if (left.left_is_red < right.left_is_red) return true;
        if (left.left_is_red > right.left_is_red) return false;
        if (left.right_is_red < right.right_is_red) return true;
        return false;
    }

    std::int64_t part1(const std::vector<std::string>& rows) {
        std::int64_t max_area{};
        const auto& pairs = parse(rows);

        for (const auto& pair1 : pairs) {
            for (const auto& pair2 : pairs) {
                auto area =
                    (std::abs(pair2.first - pair1.first) + 1) * (std::abs(pair2.second - pair1.second) + 1);
                max_area = std::max(max_area, area);
            }
        }
        
        return max_area;
    }

    std::int64_t part2(const std::vector<std::string>& rows) {
        const auto& tops = get_tops(parse(rows));

        return -1;
    }

    std::vector<TopOfRectangle> get_tops(std::vector<std::pair<int64_t, int64_t>>& pairs) {
        std::vector<TopOfRectangle> tops{};
        tops.reserve(pairs.size());

        auto& previous = pairs[0];
        for (size_t i = 1; i < pairs.size(); i++) {
            auto& current = pairs[i];
            if (current.second == previous.second) {
                const auto left(std::min(current.first, previous.first));
                const auto right(std::max(current.first, previous.first));
                tops.emplace_back(TopOfRectangle{current.second, left, right, true, true});
            }
            std::swap(previous, current);
        }

        std::sort(tops.begin(), tops.end());

        return tops;
    }

    std::vector<std::pair<int64_t, int64_t>> parse(const std::vector<std::string>& rows) {
        std::vector<std::pair<int64_t, int64_t>> pairs;
        pairs.reserve(rows.size() + 1);

        for (const auto& row : rows) {
            const auto& ints = advent::ints<int64_t>(row);
            pairs.emplace_back(std::make_pair(ints[0], ints[1]));
        }

        if (pairs.size() > 1) {
            pairs.emplace_back(pairs.front());
        }

        return pairs;
    }
}
