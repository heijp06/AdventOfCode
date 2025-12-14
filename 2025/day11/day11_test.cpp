#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day11.h"

std::vector<std::string> rows1{
    "aaa: you hhh",
    "you: bbb ccc",
    "bbb: ddd eee",
    "ccc: ddd eee fff",
    "ddd: ggg",
    "eee: out",
    "fff: out",
    "ggg: out",
    "hhh: ccc fff iii",
    "iii: out",
};

std::vector<std::string> rows2{
    "svr: aaa bbb",
    "aaa: fft",
    "fft: ccc",
    "bbb: tty",
    "tty: ccc",
    "ccc: ddd eee",
    "ddd: hub",
    "hub: fff",
    "eee: dac",
    "dac: fff",
    "fff: ggg hhh",
    "ggg: out",
    "hhh: out",
};

TEST_CASE("part1") {
    REQUIRE(day11::part1(rows1) == 5);
}

TEST_CASE("part2") {
    REQUIRE(day11::part2(rows2) == 2);
}
