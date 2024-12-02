#include <cmath>

#include "day02.h"
#include "../../lib/advent.h"

namespace day02 {
    int part1(const std::vector<std::string>& rows) {
        auto number_safe{0};

		for (const auto& row : rows) {
			const auto& report = advent::ints(row);
            number_safe += is_safe(report);
		}

        return number_safe;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int is_safe(const std::vector<int>& report) {
        if (report.size() < 2) {
            return true;
        }

        for (size_t i = 0; i < report.size() - 1; i++) {
			auto delta = std::abs(report[i] - report[i + 1]);
			if (delta < 1 || delta > 3) {
				return false;
			}

            if (report[0] < report[1]) {
				if (report[i] > report[i + 1]) {
					return false;
				}
            }
            else {
                if (report[i] < report[i + 1]) {
                    return  false;
                }
            }
        }

		return true;
    }
}
