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
        test_data_ints{"", {}}
    );

    const std::vector<int>& actual = advent::ints(item.row);

    REQUIRE(actual == item.ints);
}
