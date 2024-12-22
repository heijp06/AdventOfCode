#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day22.h"

TEST_CASE("part1") {
    std::vector<std::string> rows{
        "1",
        "10",
        "100",
        "2024"
    };

    REQUIRE(day22::part1(rows) == 37327623);
}

TEST_CASE("part2") {
    std::vector<std::string> rows{
        "1",
        "2",
        "3",
        "2024"
    };

    REQUIRE(day22::part2(rows) == 23);
}

TEST_CASE("next") {
    std::vector<int> numbers = {
        123, 15887950, 16495136, 527345, 704524, 1553684,
        12683156, 11100544, 12249484, 7753432, 5908254,
    };

    for (int i = 0; i < numbers.size() - 1; i++) {
        REQUIRE(day22::next(numbers[i]) == numbers[i + 1]);
    }
}
