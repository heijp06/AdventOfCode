#include "day11.h"
#include "day11.h"
#include "../../lib/advent.h"

namespace day11 {
    int part1(const std::vector<std::string>& rows) {
        const std::string start = "you";
        auto reactor = parse(rows, start);

        solve(rows, reactor, start);

        return reactor["out"].paths;
    }

    int part2(const std::vector<std::string>& rows) {
        const std::string start = "srv";
        auto reactor = parse(rows, start);

        solve(rows, reactor, start);

        return -1;
    }

    void solve(const std::vector<std::string>& rows, std::map<std::string, day11::Device>& reactor, std::string start) {
        std::vector<Device> current{};
        current.reserve(rows.size());
        current.emplace_back(reactor[start]);
        std::vector<Device> next{};
        next.reserve(rows.size());

        while (!current.empty()) {
            for (const auto& device : current) {
                for (const auto& output : device.outputs) {
                    auto& next_device = reactor[output];
                    next_device.pending_inputs--;
                    next_device.paths += device.paths;
                    if (!next_device.pending_inputs) {
                        next.emplace_back(next_device);
                    }
                }
            }
            std::swap(current, next);
            next.clear();
        }
    }

    std::map<std::string, Device> parse(const std::vector<std::string>& rows, std::string start)
    {
        std::map<std::string, Device> result{};

        auto out = Device{ "out", {}, {}, 0, 0, false };
        result[out.name] = out;

        for (const auto& row : rows) {
            Device device{};

            device.name = row.substr(0, 3);
            device.outputs = advent::split(row.substr(5), " ");
            device.paths = device.name == start ? 1 : 0;
            device.pending_inputs = 0;

            result[device.name] = device;
        }

        for (const auto& item : result) {
            for (const auto& output : item.second.outputs) {
                result[output].inputs.emplace_back(item.first);
            }
        }

        result[start].paths = 1;
        result[start].reachable = true;

        std::vector<std::string> current{};
        current.reserve(rows.size());
        auto next = current;
        next.reserve(rows.size());
        current.emplace_back(start);

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

        for (auto& item : result) {
            for (const auto& input : item.second.inputs) {
                item.second.pending_inputs += result[input].reachable;
            }
        }

        return result;
    }
}
