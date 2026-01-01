#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day13.h"

std::vector<std::string> rows{
    R"(/->-\        )",
    R"(|   |  /----\)",
    R"(| /-+--+-\  |)",
    R"(| | |  | v  |)",
    R"(\-+-/  \-+--/)",
    R"(  \------/   )"
};

TEST_CASE("part1") {
    REQUIRE(day13::part1(rows) == "7,3");
}

TEST_CASE("part2") {
    REQUIRE(day13::part2(rows) == -1);
}
