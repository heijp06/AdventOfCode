#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day06.h"

int main() {
    const auto& rows = advent::get_rows(2025, 6);

    std::cout << "Part 1: " << day06::part1(rows) << std::endl;
    std::cout << "Part 2: " << day06::part2(rows) << std::endl;

    return 0;
}
