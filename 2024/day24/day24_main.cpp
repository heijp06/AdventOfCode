#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day24.h"

int main() {
    const auto& rows = advent::get_rows(2024, 24);

    std::cout << "Part 1: " << day24::part1(rows) << std::endl;
    std::cout << "Part 2: " << day24::part2(rows) << std::endl;

    return 0;
}
