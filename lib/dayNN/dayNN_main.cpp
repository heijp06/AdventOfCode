#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day__NN__.h"

int main() {
	const auto& rows = advent::get_rows(2018, __N__);

	std::cout << "Part 1: " << day__NN__::part1(rows) << std::endl;
	std::cout << "Part 2: " << day__NN__::part2(rows) << std::endl;

	return 0;
}
