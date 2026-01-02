#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day13.h"

std::vector<std::string> rows1{
    R"(/->-\        )",
    R"(|   |  /----\)",
    R"(| /-+--+-\  |)",
    R"(| | |  | v  |)",
    R"(\-+-/  \-+--/)",
    R"(  \------/   )"
};

std::vector<std::string> rows2{
    R"(/>-<\  )",
    R"(|   |  )",
    R"(| /<+-\)",
    R"(| | | v)",
    R"(\>+</ |)",
    R"(  |   ^)",
    R"(  \<->/)"
};

TEST_CASE("part1") {
    REQUIRE(day13::part1(rows1) == "7,3");
}

TEST_CASE("part2") {
    REQUIRE(day13::part2(rows2) == "6,4");
}
