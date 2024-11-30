#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day.h"

int main() {
	const auto& rows = advent::get_rows(2018, );

	std::cout << "Part 1: " << day::part1(rows) << std::endl;
	std::cout << "Part 2: " << day::part2(rows) << std::endl;

	return 0;
}
