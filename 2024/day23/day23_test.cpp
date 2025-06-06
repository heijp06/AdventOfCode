#include <string>
#include <vector>

#include "../../lib/catch.hpp"
#include "day23.h"

std::vector<std::string> rows{
    "kh-tc", "qp-kh", "de-cg", "ka-co", "yn-aq", "qp-ub", "cg-tb", "vc-aq",
    "tb-ka", "wh-tc", "yn-cg", "kh-ub", "ta-co", "de-co", "tc-td", "tb-wq",
    "wh-td", "ta-ka", "td-qp", "aq-cg", "wq-ub", "ub-vc", "de-ta", "wq-aq",
    "wq-vc", "wh-yn", "ka-de", "kh-ta", "co-tc", "wh-qp", "tb-vc", "td-yn"
};

TEST_CASE("part1") {
    REQUIRE(day23::part1(rows) == 7);
}

TEST_CASE("part2") {
    REQUIRE(day23::part2(rows) == "co,de,ka,ta");
}
