#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day21.h"

struct test_data_numeric_keypad {
    char current;
    char next;
    std::string expected;
};

std::vector<std::string> rows{
    "029A",
    "980A",
    "179A",
    "456A",
    "379A"
};

TEST_CASE("part1") {
    REQUIRE(day21::part1(rows) == 126384);
}

    /*
    +---+---+---+
    | 7 | 8 | 9 |
    +---+---+---+
    | 4 | 5 | 6 |
    +---+---+---+
    | 1 | 2 | 3 |
    +---+---+---+
        | 0 | A |
        +---+---+
    */

TEST_CASE("numeric_keypad") {
    auto item = GENERATE(
        test_data_numeric_keypad{'A', 'A', "A"},
        test_data_numeric_keypad{'7', '7', "A"},
        test_data_numeric_keypad{'A', '7', "^^^<<A"},
        test_data_numeric_keypad{'7', 'A', ">>vvvA"},
        test_data_numeric_keypad{'9', '1', "vv<<A"},
        test_data_numeric_keypad{'1', '9', "^^>>A"},
        test_data_numeric_keypad{'0', '1', "^<A"},
        test_data_numeric_keypad{'1', '0', ">vA"}
    );

    day21::numeric_keypad pad;
    pad.next(item.current);

    REQUIRE(pad.next(item.next) == item.expected);
}
