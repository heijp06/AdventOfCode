#include <fstream>
#include <iostream>
#include <vector>

#include "day01.h"
#include "day01_main.h"

int main() {
	auto rows = get_rows("C:/git/AoC-Data/2018/day01.txt");

	std::cout << "Part 1: " << day01::part1(rows) << std::endl;
	std::cout << "Part 2: " << day01::part2(rows) << std::endl;

	return 0;
}

std::vector<std::string> get_rows(const std::string& path) {
	std::vector<std::string> rows{};
	std::string row;
	std::ifstream input{path};

	while (input >> row) {
		rows.push_back(row);
	}

	return rows;
}