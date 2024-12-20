#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day20.h"

struct test_data_part1 {
    int min_cheat;
    int number;
};

std::vector<std::string> rows{
"###############",
"#...#...#.....#",
"#.#.#.#.#.###.#",
"#S#...#.#.#...#",
"#######.#.#.###",
"#######.#.#...#",
"#######.#.###.#",
"###..E#...#...#",
"###.#######.###",
"#...###...#...#",
"#.#####.#.###.#",
"#.#...#.#.#...#",
"#.#.#.#.#.#.###",
"#...#...#...###",
"###############"
};

TEST_CASE("part1") {
    const auto& item = GENERATE(
            test_data_part1{2, 44},
            test_data_part1{4, 30},
            test_data_part1{6, 16},
            test_data_part1{8, 14},
            test_data_part1{10, 10},
            test_data_part1{12, 8},
            test_data_part1{20, 5},
            test_data_part1{36, 4},
            test_data_part1{38, 3},
            test_data_part1{40, 2},
            test_data_part1{64, 1}
        );

    REQUIRE(day20::part1(rows, item.min_cheat) == item.number);
}
