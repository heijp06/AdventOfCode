#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day03.h"

int main() {
	const auto& rows = advent::get_rows(2024, 3);

	std::cout << "Part 1: " << day03::part1(rows) << std::endl;
	std::cout << "Part 2: " << day03::part2(rows) << std::endl;

	return 0;
}
