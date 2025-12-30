#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day08.h"

int main() {
    const auto& rows = advent::get_rows(2018, 8);

    std::cout << "Part 1: " << day08::part1(rows) << std::endl;
    std::cout << "Part 2: " << day08::part2(rows) << std::endl;

    return 0;
}
