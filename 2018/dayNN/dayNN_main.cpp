#include <fstream>
#include <iostream>
#include <vector>

#include "../lib/advent.h"
#include "dayNN.h"

int main() {
	auto rows = advent::get_rows(2018, 0);

	std::cout << "Part 1: " << dayNN::part1(rows) << std::endl;
	std::cout << "Part 2: " << dayNN::part2(rows) << std::endl;

	return 0;
}
