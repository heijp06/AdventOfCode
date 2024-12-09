#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

struct test_data {
    std::string disk_map;
    int last_file_number;
};

std::vector<std::string> rows = {
    "2333133121414131402"
};

//TEST_CASE("part1") {
//    REQUIRE(day09::part1(rows) == 1928);
//}

TEST_CASE("parse") {
    REQUIRE(day09::parse("12345") == std::vector<int>{1, 2, 3, 4, 5});
}

TEST_CASE("last file number") {
    const auto& item = GENERATE(
        test_data{"12345", 2},
        test_data{rows[0], 9}
    );

    const auto& compactor = day09::compactor(item.disk_map);

    REQUIRE(compactor.get_last_file_number() == item.last_file_number);
}
