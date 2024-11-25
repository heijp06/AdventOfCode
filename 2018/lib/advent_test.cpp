#include <vector>
#include <string>

#include "catch.hpp"
#include "advent.h"

struct test_data_ints {
    std::string row;
    std::vector<int> ints;
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
