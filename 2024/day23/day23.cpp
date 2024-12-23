#include <stdexcept>
#include <algorithm>

#include "day23.h"
#include "../../lib/advent.h"

namespace day23 {
    int part1(const std::vector<std::string>& rows) {
        const auto& pairs = get_pairs(rows);
        const auto& triples = get_triples(pairs);

        auto counter{0};
        for (const auto& triple : triples) {
            for (const auto& value : triple) {
                if (value[0] == 't') {
                    counter++;
                    break;
                }
            }
        }

        return counter;
    }

    std::string part2(const std::vector<std::string>& rows) {
        const auto& pairs = get_pairs(rows);
        auto lan_parties = get_triples(pairs);

        while (true) {
            std::vector<std::vector<std::string>> new_lan_parties;
            for (const auto& lan_party : lan_parties) {
                const auto& first_member = *(lan_party.cbegin());
                for (const auto& new_member : pairs.at(first_member)) {
                    // Check if new member is really new
                    if (std::find(lan_party.cbegin(), lan_party.cend(), new_member) != lan_party.cend()) {
                        continue;
                    }

                    // Only add at the end to prevent duplicates.
                    if (*(lan_party.crbegin()) > new_member) {
                        continue;
                    }

                    // All members of the lan party should be friends with the new member.
                    auto found{true};
                    for (auto it = ++lan_party.cbegin(); it != lan_party.cend(); it++) {
                        const auto& old_member = *it;

                        if (new_member < old_member) {
                            if (!pairs.count(new_member)) {
                                found = false;
                                break;
                            }
                            const auto& new_friends = pairs.at(new_member);
                            if (std::find(
                                new_friends.cbegin(), new_friends.cend(), old_member) == new_friends.cend()) {
                                found = false;
                                break;
                            }
                        }

                        if (old_member < new_member) {
                            if (!pairs.count(old_member)) {
                                found = false;
                                break;
                            }
                            const auto& old_friends = pairs.at(old_member);
                            if (std::find(
                                old_friends.cbegin(), old_friends.cend(), new_member) == old_friends.cend()) {
                                found = false;
                                break;
                            }
                        }
                    }

                    if (found) {
                        // A true friend! Add it to the party.
                        auto new_lan_party = std::vector<std::string>(lan_party);
                        new_lan_party.push_back(new_member);
                        new_lan_parties.push_back(new_lan_party);
                    }
                }
            }
            if (!new_lan_parties.size()) {
                if (!lan_parties.size()) {
                    throw std::domain_error("Not found!");
                }

                std::string result;
                const auto& lan_party = *(lan_parties.cbegin());
                for (const auto& member : lan_party) {
                    if (result.size()) {
                        result += ",";
                    }
                    result += member;
                }
                return result;
            }
            lan_parties = new_lan_parties;
        }

        throw std::domain_error("Not found!");
    }

    std::vector<std::vector<std::string>> get_triples(std::unordered_map<std::string, std::vector<std::string>> pairs) {
        std::vector<std::vector<std::string>> triples;

        for (const auto& [value1, values1] : pairs) {
            for (const auto& value2 : values1) {
                if (pairs.count(value2)) {
                    const auto& values2 = pairs[value2];
                    for (const auto& value3 : values2) {
                        const auto& it = std::find(values1.cbegin(), values1.cend(), value3);
                        if (it != values1.cend()) {
                            triples.push_back({value1, value2, value3});
                        }
                    }
                }
            }
        }

        return triples;
    }

    std::unordered_map<std::string, std::vector<std::string>> get_pairs(const std::vector<std::string>& rows) {
        std::unordered_map<std::string, std::vector<std::string>> pairs;

        for (const auto& row : rows) {
            const auto& pair = advent::split(row, "-");
            if (pair[0] < pair[1]) {
                pairs[pair[0]].push_back(pair[1]);
            }
            else {
                pairs[pair[1]].push_back(pair[0]);
            }
        }

        return pairs;
    }
}
