#pragma once

#include <queue>
#include <string>
#include <vector>

namespace day09 {
    int64_t part1(const std::vector<std::string>& rows);
    int64_t part2(const std::vector<std::string>& rows);

    struct block {
        int id;
        int length;

        friend bool operator==(const block& left, const block& right) {
            return left.id == right.id && left.length == right.length;
        }
    };

    std::vector<int> parse(const std::string& row);

    class compactor {
    public:
        explicit compactor(const std::string& disk_map);
        int get_last_file_number() const;
        int get_file_length() const;
        int read();

    private:
        struct read_state {
            int index;
            int counter;
        };
        std::vector<int> layout_;
        read_state front_;
        read_state back_;
        int index_;
        int free_space_;
    };

    struct block2 {
        int index;
        int id;
        int length;
    };

    using queue_t = std::priority_queue<int, std::vector<int>, std::greater<>>;

    struct part2_data {
        std::vector<block2> blocks;
        std::vector<queue_t> queues;
    };

    part2_data parse2(const std::string& row);
}
