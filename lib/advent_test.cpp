#include <string>
#include <utility>
#include <vector>

#include "catch.hpp"
#include "advent.h"

struct test_data_ints {
    std::string row;
    std::vector<int> ints;
};

struct test_data_longs {
    std::string row;
    std::vector<int64_t> ints;
};

struct test_data_split {
    std::string text;
    std::string delimiter;
    std::vector<std::string> expected;
};

TEST_CASE("changes") {
    const auto item = GENERATE(
        test_data_ints{"", {}},
        test_data_ints{"1", {1}},
        test_data_ints{"+1", {1}},
        test_data_ints{"0", {0}},
        test_data_ints{"-1", {-1}},
        test_data_ints{"-1,0,1", {-1, 0, 1}},
        test_data_ints{"asd12asd+12,,-4,assd--ad-+f+5  0 1 2", {12, 12, -4, 5, 0, 1, 2}}
    );

    const std::vector<int>& actual = advent::ints(item.row);

    REQUIRE(actual == item.ints);
}

TEST_CASE("longs") {
    const auto item = GENERATE(
        test_data_longs{"", {}},
        test_data_longs{"10000000000", {10000000000}},
        test_data_longs{"+10000000000", {10000000000}},
        test_data_longs{"0", {0}},
        test_data_longs{"-10000000000", {-10000000000}},
        test_data_longs{"-1", {-1}},
        test_data_longs{"-1,0,1", {-1, 0, 1}},
        test_data_longs{"asd12asd+12,,-4,assd--ad-+f+5  0 1 2", {12, 12, -4, 5, 0, 1, 2}}
    );

    const std::vector<int64_t>& actual = advent::ints<int64_t>(item.row);

    REQUIRE(actual == item.ints);
}

TEST_CASE("grid") {
    const std::vector<std::string> rows = {"ABC", "DII"};

    const auto& grid = advent::grid(rows);

    REQUIRE(grid.get_height() == 2);
    REQUIRE(grid.get_width() == 3);

    REQUIRE(grid[{0, 0}] == 'A');
    REQUIRE(grid[{0, 1}] == 'B');
    REQUIRE(grid[{0, 2}] == 'C');
    REQUIRE(grid[{1, 0}] == 'D');
    REQUIRE(grid[{1, 1}] == 'I');
    REQUIRE(grid[{1, 2}] == 'I');

    REQUIRE(grid.find('C') == advent::coord{0, 2});
    REQUIRE(grid.find('I') == advent::coord{1, 1});
    REQUIRE_THROWS(grid.find('X'));

    REQUIRE(grid.find_all('C') == std::vector<advent::coord>({{0, 2}}));
    REQUIRE(grid.find_all('I') == std::vector<advent::coord>({{1, 1}, {1, 2}}));
    REQUIRE(grid.find_all('X') == std::vector<advent::coord>());
}

TEST_CASE("mutable grid") {
    const std::vector<std::string> rows = {"ABC", "DEF"};

    auto grid = advent::grid(rows);

    REQUIRE(grid.get_height() == 2);
    REQUIRE(grid.get_width() == 3);

    REQUIRE(grid[{0, 0}] == 'A');
    REQUIRE(grid[{0, 1}] == 'B');
    REQUIRE(grid[{0, 2}] == 'C');
    REQUIRE(grid[{1, 0}] == 'D');
    REQUIRE(grid[{1, 1}] == 'E');
    REQUIRE(grid[{1, 2}] == 'F');

    grid[{0, 1}] = 'X';
    grid[{1, 2}] = 'Y';

    REQUIRE(grid[{0, 0}] == 'A');
    REQUIRE(grid[{0, 1}] == 'X');
    REQUIRE(grid[{0, 2}] == 'C');
    REQUIRE(grid[{1, 0}] == 'D');
    REQUIRE(grid[{1, 1}] == 'E');
    REQUIRE(grid[{1, 2}] == 'Y');
}

TEST_CASE("empty grid") {
    const std::vector<std::string> rows{};

    const auto& grid = advent::grid(rows);

    REQUIRE(grid.get_height() == 0);
    REQUIRE(grid.get_width() == 0);
}

TEST_CASE("directions") {
    REQUIRE(advent::direction::up() == advent::direction{-1, 0});
    REQUIRE(advent::direction::down() == advent::direction{1, 0});
    REQUIRE(advent::direction::left() == advent::direction{0, -1});
    REQUIRE(advent::direction::right() == advent::direction{0, 1});
}

TEST_CASE("coord + direction") {
    auto position = advent::coord{1, 1};

    REQUIRE(position + advent::direction::up() == advent::coord{0, 1});
    REQUIRE(position + advent::direction::down() == advent::coord{2, 1});
    REQUIRE(position + advent::direction::left() == advent::coord{1, 0});
    REQUIRE(position + advent::direction::right() == advent::coord{1, 2});

    position += advent::direction::up();

    REQUIRE(position == advent::coord{0, 1});
}

TEST_CASE("split") {
    const auto item = GENERATE(
        test_data_split{"", "", {}},
        test_data_split{"", ",", {}},
        test_data_split{"a", "", {"a"}},
        test_data_split{"abc", "", {"a", "b", "c"}},
        test_data_split{"a,b,c", ",", {"a", "b", "c"}},
        test_data_split{"axxxbxxxc", "xx", {"a", "xb", "xc"}},
        test_data_split{"axxxxbxxxxxc", "xx", {"a", "", "b", "", "xc"}}
    );

    REQUIRE(advent::split(item.text, item.delimiter) == item.expected);
}

//TEST_CASE("direction.turn") {
//    REQUIRE(advent::direction.up().turn_left() == advent::direction::left());
//    REQUIRE(advent::direction.up().turn_right() == advent::direction::right());
//    REQUIRE(advent::direction.left().turn_left() == advent::direction::down());
//    REQUIRE(advent::direction.left().turn_right() == advent::direction::up());
//}
