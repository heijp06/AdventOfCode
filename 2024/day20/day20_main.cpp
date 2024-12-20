#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day20.h"

int main() {
    const auto& rows = advent::get_rows(2024, 20);

    std::cout << "Part 1: " << day20::part1(rows) << std::endl;
    std::cout << "Part 2: " << day20::part2(rows) << std::endl;

    return 0;
}
