#include <algorithm>
#include <cmath>
#include <iostream>
#include <stdexcept>

#include "day09.h"
#include "../../lib/advent.h"

namespace day09 {
    bool static operator==(const Segment& left, const Segment& right) {
        return
            left.row == right.row &&
            left.left == right.left &&
            left.right == right.right &&
            left.left_type == right.left_type &&
            left.right_type == right.right_type;
    }

    bool static operator!=(const Segment& left, const Segment& right) {
        return !(left == right);
    }

    bool static operator<(const Segment& left, const Segment& right) {
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

    std::ostream& operator<<(std::ostream& os, const Segment& segment) {
        os << segment.row << " " << segment.left << " " << segment.left_type << " ";
        os << segment.right << " " << segment.right_type;
        return os;
    }

    std::int64_t Segment::length() const {
        return right - left + 1;
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
        const auto& segments = get_segments(parse(rows));
        std::vector<Segment> current{};
        current.reserve(segments.size() * 2);
        std::vector<Segment> next{};
        next.reserve(segments.size() * 2);
        std::int64_t max_area{};

        for (const auto& new_segment : segments) {
            max_area = std::max(max_area, new_segment.length());
            auto add{true};
            for (const auto& segment : current) {
                if (new_segment.right < segment.left || new_segment.left > segment.right) {
                    next.push_back(segment);
                    continue;
                }

                if (new_segment.left <= segment.left) {
                    if (new_segment.right >= segment.right) {
                        if (new_segment.left == segment.left && new_segment.left_is_red && segment.right_is_red ||
                            new_segment.right == segment.right && new_segment.right_is_red && segment.left_is_red) {
                            max_area =
                                std::max(max_area, segment.length() * (new_segment.row - segment.row + 1));
                        }
                        add = false;
                        continue;
                    }
                }
            }
            if (add) {
                next.push_back(new_segment);
            }
            std::swap(current, next);
            next.clear();
        }

        return max_area;
    }

    std::vector<Segment> get_segments(std::vector<std::pair<int64_t, int64_t>>& pairs) {
        std::vector<Segment> segments{};
        segments.reserve(pairs.size());

        for (size_t i = 1; i < pairs.size(); i++) {
            const auto& start = pairs[i - 1];
            const auto& end = pairs[i];
            if (end.second == start.second) {
                const auto& previous = pairs[(i + pairs.size() - 2) % pairs.size()];
                const auto& next = pairs[(i + 1) % pairs.size()];
                EndType start_type = previous.second < start.second ? EndType::Bottom : EndType::Top;
                EndType end_type = end.second < next.second ? EndType::Top : EndType::Bottom;
                if (start.first < end.first) {
                    segments.emplace_back(Segment{end.second, start.first, end.first, start_type, end_type, true, true});
                }
                else {
                    segments.emplace_back(Segment{end.second, end.first, start.first, end_type, start_type, true, true});
                }
            }
        }

        std::sort(segments.begin(), segments.end());

        return segments;
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
