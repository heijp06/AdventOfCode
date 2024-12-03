#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "../../lib/advent.h"
#include "day03.h"

namespace day03 {
    int part1(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    int mul(const std::string& row) {
        auto result{0};
		std::regex int_regex{R"(mul(\d+,\d+)"};
		const auto& begin = std::sregex_iterator(row.cbegin(), row.cend(), int_regex);
		const auto& end = std::sregex_iterator();

		for (auto it = begin; it != end; ++it) {
			int value;
			const auto& match = *it;
            auto ints = advent::ints(match.str());
            result += ints[0] * ints[1];
   //         std::istringstream rs{match.str()};
   //         rs >> value;
			//result.push_back(value);
		}

		return result;
    }
}
