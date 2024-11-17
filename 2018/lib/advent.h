#pragma once

#include <filesystem>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace advent {
	std::vector<std::string> get_rows(int year, int day);
	std::string get_data_path(int year, int day);

	// Read the input file for @year and @day as a vector of strings
	std::vector<std::string> get_rows(int year, int day) {
		auto path{get_data_path(year, day)};
		std::vector<std::string> rows{};
		std::string row;
		std::ifstream input{path};

		while (input >> row) {
			rows.push_back(row);
		}

		return rows;
	}

	// Get the path to the input file for @year and @day
	std::string get_data_path(int year, int day) {
		const std::string data_folder{"data"};
		auto year_folder{std::to_string(year)};
		std::ostringstream ss;
		ss << "day" << std::setw(2) << std::setfill('0') << day << ".txt";
		std::string day_file(ss.str());

		auto current{std::filesystem::current_path()};
		while (!std::filesystem::exists(current / data_folder)) {
			if (!current.has_parent_path()) {
				throw std::domain_error("Cannot find data.");
			}
			current = current.parent_path();
		}

		auto input = current / data_folder / year_folder / day_file;

		return input.string();
	}
}
