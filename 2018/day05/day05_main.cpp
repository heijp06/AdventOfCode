#include <fstream>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day05.h"

int main() {
    const auto& rows = advent::get_rows(2018, 5);

    std::cout << "Part 1: " << day05::part1(rows) << std::endl;
    std::cout << "Part 2: " << day05::part2(rows) << std::endl;

    return 0;
}
