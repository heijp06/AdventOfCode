#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day15.h"

int main() {
    const auto& rows = advent::get_rows(2024, 15);

    std::cout << "Part 1: " << day15::part1(rows) << std::endl;
    std::cout << "Part 2: " << day15::part2(rows) << std::endl;

    return 0;
}
