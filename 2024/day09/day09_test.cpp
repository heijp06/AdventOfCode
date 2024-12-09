#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

std::vector<std::string> rows = {
    "2333133121414131402"
};

//TEST_CASE("part1") {
//    REQUIRE(day09::part1(rows) == 1928);
//}

TEST_CASE("parse") {
    REQUIRE(day09::parse("12345") == std::vector<int>{1, 2, 3, 4, 5});
}
