#include <regex>
#include <string>
#include <vector>

#include "../../lib/advent.h"
#include "day03.h"

namespace day03 {
    int part1(const std::vector<std::string>& rows) {
		auto result{0};

		for (const auto& row : rows) {
			result += mul(row);
		}

        return result;
    }

    int part2(const std::vector<std::string>& rows) {
		auto result{0};

		std::string line;
		for (const auto& row : rows) {
			line += row;
			line += " ";
		}

		return mul(line, false);
    }

    int mul(const std::string& row, bool part1) {
        auto result{0};
		std::regex int_regex{R"---(do(n't)?\(\)|mul\(\d{1,3},\d{1,3}\))---"};
		const auto& begin = std::sregex_iterator(row.cbegin(), row.cend(), int_regex);
		const auto& end = std::sregex_iterator();
		auto add{true};

		for (auto it = begin; it != end; ++it) {
			const auto& match = *it;

			if (match.str() == "do()") {
				add = true;
				continue;
			}

			if (match.str() == "don't()") {
				add = false;
				continue;
			}

			if (part1 || add) {
				auto ints = advent::ints(match.str());
				result += ints[0] * ints[1];
			}
		}

		return result;
    }
}
