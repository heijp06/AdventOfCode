#pragma once

#include <string>
#include <vector>

namespace advent {
	std::vector<std::string> get_rows(int year, int day);
	std::string get_data_path(int year, int day);
	std::vector<int> ints(std::string row);
}

