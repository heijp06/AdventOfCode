#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day17.h"

std::vector<std::string> rows {
    "Register A: 729",
    "Register B: 0",
    "Register C: 0",
    "",
    "Program: 0,1,5,4,3,0"
};

TEST_CASE("part1") {
    REQUIRE(day17::part1(rows) == "4,6,3,5,6,3,5,2,1,0");
}

TEST_CASE("example 1") {
    auto comp = day17::computer(0, 0, 9, {2, 6});
    comp.run();

    REQUIRE(comp.b() == 1);
}

TEST_CASE("example 2") {
    auto comp = day17::computer(10, 0, 0, {5, 0, 5, 1, 5, 4});
    auto output = comp.run();

    REQUIRE(output == "0,1,2");
}

TEST_CASE("example 3") {
    auto comp = day17::computer(2024, 0, 0, {0, 1, 5, 4, 3, 0});
    auto output = comp.run();

    REQUIRE(comp.a() == 0);
    REQUIRE(output == "4,2,5,6,7,7,7,7,3,1,0");
}

TEST_CASE("example 4") {
    auto comp = day17::computer(0, 29, 0, {1, 7});
    comp.run();

    REQUIRE(comp.b() == 26);
}

TEST_CASE("example 5") {
    auto comp = day17::computer(0, 2024, 43690, {4, 0});
    comp.run();

    REQUIRE(comp.b() == 44354);
}
