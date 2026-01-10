#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day17.h"

int main() {
    const auto& rows = advent::get_rows(2018, 17);

    std::cout << "Part 1: " << day17::part1(rows) << std::endl;
    std::cout << "Part 2: " << day17::part2(rows) << std::endl;

    return 0;
}
