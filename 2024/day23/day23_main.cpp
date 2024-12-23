#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day23.h"

int main() {
    const auto& rows = advent::get_rows(2024, 23);

    std::cout << "Part 1: " << day23::part1(rows) << std::endl;
    std::cout << "Part 2: " << day23::part2(rows) << std::endl;

    return 0;
}
