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
            left.left_is_red == right.left_is_red &&
            left.right_is_red == right.right_is_red;
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
        if (left.left_is_red < right.left_is_red) return true;
        if (left.left_is_red < right.left_is_red) return false;
        if (left.right_is_red < right.right_is_red) return true;
        if (left.right_is_red < right.right_is_red) return false;
        return false;
    }

    std::ostream& operator<<(std::ostream& os, const Segment& segment) {
        os << segment.row << " " << segment.left << " " << segment.right;
        os << " " << segment.left_is_red << " " << segment.right_is_red;
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

    // 8098967: Too low.
    std::int64_t part2(const std::vector<std::string>& rows) {
        const auto& segments = get_segments(parse(rows));
        std::vector<Segment> current{};
        current.reserve(segments.size() * 2);
        std::vector<Segment> next{};
        next.reserve(segments.size() * 2);
        std::int64_t max_area{};
        std::vector<Segment> to_add{};
        to_add.reserve(2);

        for (const auto& new_segment : segments) {
            ////std::cout << new_segment << std::endl;
            max_area = std::max(max_area, new_segment.length());
            to_add.push_back(new_segment);
            for (const auto& segment : current) {
                ////std::cout << '\t' << segment << std::endl;
                if (new_segment.right <= segment.left || new_segment.left >= segment.right) {
                    next.push_back(segment);
                    if (new_segment.right == segment.left && segment.left_is_red) {
                        max_area = std::max(max_area, new_segment.row - segment.row + 1);
                        to_add.push_back({new_segment.row, new_segment.left, segment.right, true, segment.right_is_red});
                    } else if (new_segment.left == segment.right && segment.right_is_red) {
                        max_area = std::max(max_area, new_segment.row - segment.row + 1);
                        to_add.push_back({new_segment.row, segment.left, new_segment.right, segment.left_is_red, true});
                    }
                    continue;
                }

                to_add.clear();

                if (new_segment.left <= segment.left) {
                    if (new_segment.right >= segment.right) {
                        if (new_segment.left == segment.left && segment.right_is_red ||
                            new_segment.right == segment.right && segment.left_is_red) {
                            max_area =
                                std::max(max_area, segment.length() * (new_segment.row - segment.row + 1));
                        }
                    }
                    else {
                        if (segment.left_is_red) {
                            max_area = std::max(max_area, (new_segment.right - segment.left + 1) * (new_segment.row - segment.row + 1));
                        }
                        next.push_back({segment.row, segment.left, new_segment.right, segment.left_is_red, false});
                    }
                }
                else {
                    if (new_segment.right >= segment.right) {
                        if (segment.right_is_red) {
                            max_area = std::max(max_area, (segment.right - new_segment.left + 1) * (new_segment.row - segment.row + 1));
                        }
                        next.push_back({segment.row, new_segment.left, segment.right, false, segment.right_is_red});
                    }
                    else {
                        next.push_back({segment.row, segment.left, new_segment.left, segment.left_is_red, false});
                        next.push_back({segment.row, new_segment.right, segment.right, false, segment.right_is_red});
                    }
                }
            }
            next.insert(next.end(), to_add.cbegin(), to_add.cend());
            std::swap(current, next);
            next.clear();
            to_add.clear();
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
                if (start.first < end.first) {
                    segments.emplace_back(Segment{end.second, start.first, end.first, true, true});
                }
                else {
                    segments.emplace_back(Segment{end.second, end.first, start.first, true, true});
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
