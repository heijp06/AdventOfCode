#include <fstream>
#include <iostream>
#include <vector>

#include "../lib/advent.h"
#include "day01.h"

int main() {
	const auto& rows = advent::get_rows(2018, 1);

	std::cout << "Part 1: " << day01::part1(rows) << std::endl;
	std::cout << "Part 2: " << day01::part2(rows) << std::endl;

	return 0;
}