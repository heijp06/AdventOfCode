#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day14.h"

int main() {
    const auto& rows = advent::get_rows(2024, 14);

    std::cout << "Part 1: " << day14::part1(rows) << std::endl;
    std::cout << "Part 2:" << std::endl << std::endl;
    std::cout << std::endl << day14::part2(rows) << std::endl;

    return 0;
}
