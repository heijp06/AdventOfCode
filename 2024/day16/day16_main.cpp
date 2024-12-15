#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day16.h"

int main() {
    const auto& rows = advent::get_rows(2024, 16);

    std::cout << "Part 1: " << day16::part1(rows) << std::endl;
    std::cout << "Part 2: " << day16::part2(rows) << std::endl;

    return 0;
}
