#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    int part1(const std::vector<std::string>& rows) {
        auto reactor = parse(rows);

        return -1;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::map<std::string, Device> parse(const std::vector<std::string>& rows)
    {
        std::map<std::string, Device> result;

        for (const auto& row : rows) {
            Device device{};

            device.name = row.substr(0, 3);
            device.outputs = advent::split(row.substr(5), " ");
            device.paths = 0;
            device.pending_inputs = 0;

            result[device.name] = device;
        }

        result["you"].paths = 1;
        result["you"].reachable = true;

        std::vector<std::string> current{};
        current.reserve(rows.size());
        auto next = current;
        current.emplace_back("you");

        while (!current.empty()) {
            for (const auto& d : current) {
                for (const auto& o : result[d].outputs) {
                    if (result[o].reachable) {
                        continue;
                    }

                    result[o].reachable = true;
                    next.emplace_back(result[o].name);
                }
            }
            std::swap(current, next);
            next.clear();
        }

        return result;
    }
}
