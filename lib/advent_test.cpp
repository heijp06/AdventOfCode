#include <vector>
#include <string>

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

TEST_CASE("changes") {
    const auto& item = GENERATE(
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
    const auto& item = GENERATE(
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
    const std::vector<std::string> rows = {"ABC", "DEF"};

    const auto& grid = advent::grid(rows);

    REQUIRE(grid.get_height() == 2);
    REQUIRE(grid.get_width() == 3);
}
