#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day09.h"

struct test_data {
    std::string disk_map;
    int value;
};

struct test_data_compacting {
    std::string disk_map;
    std::vector<int> compacted;
};

std::vector<std::string> rows = {
    "2333133121414131402"
};

TEST_CASE("part1") {
    REQUIRE(day09::part1(rows) == 1928);
}

TEST_CASE("parse") {
    REQUIRE(day09::parse("12345") == std::vector<int>{1, 2, 3, 4, 5});
}

TEST_CASE("last file number") {
    const auto& item = GENERATE(
        test_data{"12345", 2},
        test_data{rows[0], 9}
    );

    const auto& compactor = day09::compactor(item.disk_map);

    REQUIRE(compactor.get_last_file_number() == item.value);
}

TEST_CASE("file_length") {
    const auto& item = GENERATE(
        test_data{"12345", 9},
        test_data{rows[0], 28}
    );

    const auto& compactor = day09::compactor(item.disk_map);

    REQUIRE(compactor.get_file_length() == item.value);
}

TEST_CASE("compact") {
    const auto& item = GENERATE(
        test_data_compacting{"12345", { 0, 2, 2, 1, 1, 1, 2, 2, 2 }},
        test_data_compacting{rows[0], { 0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6 }}
    );

    day09::compactor c{item.disk_map};
    std::vector<int> result;

    for (size_t i = 0; i < c.get_file_length(); i++) {
        result.push_back(c.read());
    }

    REQUIRE(result == item.compacted);
}

TEST_CASE("parse2") {
    std::vector<day09::block> expected = {
        {0, 2}, {-1, 3}, {1, 3}, {-1, 3}, {2, 1}, {-1, 3}, {3, 3}, {-1, 1},
        {4, 2}, {-1, 1}, {5, 4}, {-1, 1}, {6, 4}, {-1, 1}, {7, 3}, {-1, 1},
        {8, 4}, {9, 2}
    };

    auto actual = day09::parse2(rows[0]);

    REQUIRE(actual == expected);
}

TEST_CASE("parse3") {
    std::vector<int> expected = {
        0, 0, -1, -1, -1, 1, 1, 1, -1, -1, -1, 2, -1, -1, -1, 3, 3, 3, -1, 4,
        4, -1, 5, 5, 5, 5, -1, 6, 6, 6, 6, -1, 7, 7, 7, -1, 8, 8, 8, 8, 9, 9
    };

    const auto& actual = day09::parse3(rows[0]);

    REQUIRE(actual == expected);
}
