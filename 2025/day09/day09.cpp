#include <algorithm>
#include <cmath>
#include <iostream>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    bool static operator==(const TopOfRectangle& left, const TopOfRectangle& right) {
        return
            left.row == right.row &&
            left.left == right.left &&
            left.right == right.right &&
            left.left_type == right.left_type &&
            left.right_type == right.right_type;
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
        if (left.left_type < right.left_type) return true;
        if (left.left_type > right.left_type) return false;
        if (left.right_type < right.right_type) return true;
        return false;
    }

    std::ostream& operator<<(std::ostream& os, const EndType& end_type) {
        switch (end_type) {
        case EndType::Top:
            os << "Top";
            break;
        case EndType::Middle:
            os << "Middle";
            break;
        case EndType::Bottom:
            os << "Bottom";
            break;
        }
        return os;
    }

    std::ostream& operator<<(std::ostream& os, const TopOfRectangle& segment) {
        os << segment.row << " " << segment.left << " " << segment.left_type << " ";
        os << segment.right << " " << segment.right_type;
        return os;
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
        int max_area{};

        for (const auto& top : tops) {
            std::cout << top << std::endl;
        }

        return max_area;
    }

    std::vector<TopOfRectangle> get_tops(std::vector<std::pair<int64_t, int64_t>>& pairs) {
        std::vector<TopOfRectangle> tops{};
        tops.reserve(pairs.size());

        for (size_t i = 1; i < pairs.size(); i++) {
            const auto& start = pairs[i - 1];
            const auto& end = pairs[i];
            if (end.second == start.second) {
                const auto& previous = pairs[(i + pairs.size() - 2) % pairs.size()];
                const auto& next = pairs[(i + 1) % pairs.size()];
                EndType start_type = previous.second < start.second ? EndType::Bottom : EndType::Top;
                EndType end_type = end.second < next.second ? EndType::Top : EndType::Bottom;
                if (start.first < end.first) {
                    tops.emplace_back(TopOfRectangle{end.second, start.first, end.first, start_type, end_type});
                }
                else {
                    tops.emplace_back(TopOfRectangle{end.second, end.first, start.first, end_type, start_type});
                }
            }
        }

        std::sort(tops.begin(), tops.end());

        return tops;
    }

    std::vector<std::pair<int64_t, int64_t>> parse(const std::vector<std::string>& rows) {
        std::vector<std::pair<int64_t, int64_t>> pairs;
        pairs.reserve(rows.size());

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
