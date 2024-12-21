#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day21.h"

struct test_data_keypad {
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
        test_data_keypad{'A', 'A', "A"},
        test_data_keypad{'7', '7', "A"},
        test_data_keypad{'A', '7', "^^^<<A"},
        test_data_keypad{'7', 'A', ">>vvvA"},
        test_data_keypad{'9', '1', "vv<<A"},
        test_data_keypad{'1', '9', "^^>>A"},
        test_data_keypad{'0', '1', "^<A"},
        test_data_keypad{'1', '0', ">vA"}
    );

    day21::numeric_keypad pad;
    pad.next(item.current);

    REQUIRE(pad.next(item.next) == item.expected);
}

    /*
        +---+---+
        | ^ | A |
    +---+---+---+
    | < | v | > |
    +---+---+---+
    */

TEST_CASE("directional_keypad") {
    auto item = GENERATE(
        test_data_keypad{'A', 'A', "A"},
        test_data_keypad{'<', '<', "A"},
        test_data_keypad{'A', '<', "v<<A"},
        test_data_keypad{'<', 'A', ">>^A"},
        test_data_keypad{'^', '>', "v>A"},
        test_data_keypad{'>', '^', "^<A"},
        test_data_keypad{'^', '<', "v<A"},
        test_data_keypad{'<', '^', ">^A"}
    );

    day21::directional_keypad pad;
    pad.next(item.current);

    REQUIRE(pad.next(item.next) == item.expected);
}
