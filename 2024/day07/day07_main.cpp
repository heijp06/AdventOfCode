#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day07.h"

int main() {
	const auto& rows = advent::get_rows(2024, 7);

	std::cout << "Part 1: " << day07::part1(rows) << std::endl;
	std::cout << "Part 2: " << day07::part2(rows) << std::endl;

	return 0;
}
