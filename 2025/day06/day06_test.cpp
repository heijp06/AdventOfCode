#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day06.h"

std::vector<std::string> rows{
"123 328  51 64 ",
" 45 64  387 23 ",
"  6 98  215 314",
"*   +   *   +  ",
};

TEST_CASE("part1") {
    REQUIRE(day06::part1(rows) == -1);
}
