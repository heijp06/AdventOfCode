#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day04.h"

int main() {
	const auto& rows = advent::get_rows(2024, 4);

	std::cout << "Part 1: " << day04::part1(rows) << std::endl;
	std::cout << "Part 2: " << day04::part2(rows) << std::endl;

	return 0;
}
