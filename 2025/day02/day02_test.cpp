#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day02.h"

std::vector<std::string> rows{"11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"};

TEST_CASE("part1") {
    REQUIRE(day02::part1(rows) == 1227775554L);
}
