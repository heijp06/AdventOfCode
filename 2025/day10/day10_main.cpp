#include <chrono>
#include <iostream>
#include <vector>

#include "../../lib/advent.h"
#include "day10.h"

int main() {
    const auto& rows = advent::get_rows(2025, 10);

    auto start = std::chrono::steady_clock::now();

    std::cout << "Part 1: " << day10::part1(rows) << std::endl;
    std::cout << "Part 2: " << day10::part2(rows) << std::endl;

    auto end = std::chrono::steady_clock::now();
    const std::chrono::duration<double> elapsed_seconds{end - start};
    std::cout << elapsed_seconds.count() << std::endl;

    return 0;
}
