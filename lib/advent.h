#pragma once

#include <regex>
#include <sstream>
#include <string>
#include <vector>

namespace advent {
	std::vector<std::string> get_rows(int year, int day);
	std::string get_data_path(int year, int day);
	std::vector<int> ints(const std::string& row);

	// Get all the integers of type T from a string.
	template<typename T>
	std::vector<T> ints(const std::string& row) {
		std::vector<T> result{};
		std::regex int_regex{R"([+-]?\d+)"};
		const auto& begin = std::sregex_iterator(row.cbegin(), row.cend(), int_regex);
		const auto& end = std::sregex_iterator();

		for (auto it = begin; it != end; ++it) {
			T value;
			const auto& match = *it;
            std::istringstream rs{match.str()};
            rs >> value;
			result.push_back(value);
		}

		return result;
	}

    struct coord {
        int row;
        int column;

        friend bool operator==(const coord& l, const coord& r) {
            return l.row == r.row && l.column == r.column;
        }
        friend bool operator<(const coord& l, const coord& r) {
            return l.row == r.row ? l.column < r.column : l.row < r.row;
        }
        friend coord operator+(const coord& l, const coord& r) {
            return {l.row + r.row, l.column + r.column};
        }
        friend coord operator-(const coord& l, const coord& r) {
            return {l.row - r.row, l.column - r.column};
        }
        friend coord operator-(const coord& c) {
            return {-c.row, -c.column};
        }
    };
}

