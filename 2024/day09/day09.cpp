#include "day09.h"

namespace day09 {
    int64_t part1(const std::vector<std::string>& rows) {
        int64_t checksum{0};
        auto c = compactor{rows[0]};
        auto length = c.get_file_length();

        for (int64_t i = 0; i < length; i++) {
            checksum += i * c.read();
        }

        return checksum;
    }

    // 10898720530938:  Too high.
    // 8654184283366:   Too high.
    int64_t part2(const std::vector<std::string>& rows) {
        auto data = parse2(rows[0]);

        for (auto it = data.blocks.rbegin(); it != data.blocks.rend(); ++it) {
            auto& block = *it;
            auto index{-1};
            auto queue_index{-1};
            for (auto i = block.length; i < 10; i++) {
                auto& queue = data.queues[i];
                if (!queue.empty() && (index < 0 || index > queue.top())) {
                    index = queue.top();
                    queue_index = i;
                }
            }
            if (index >= 0) {
                auto& queue = data.queues[queue_index];
                queue.pop();
                block.index = index;
                if (queue_index > block.length) {
                    data.queues[queue_index - block.length].push(index + block.length);
                }
            }
        }

        int64_t result{0};

        for (const auto& block: data.blocks) {
            for (int i = 0; i < block.length; i++) {
                result += block.id * (block.index + i);
            }
        }

        return result;
    }

    std::vector<int> parse(const std::string& row) {
        std::vector<int> result;

        for (const auto c : row) {
            result.push_back(c - '0');
        }

        return result;
    }

    compactor::compactor(const std::string& disk_map) :
	    layout_{parse(disk_map)},
	    front_{0, layout_[0]},
	    back_{static_cast<int>(layout_.size()) - 1, layout_.back()},
        index_(0),
        free_space_{0} {
    }

    int compactor::get_last_file_number() const {
        return static_cast<int>(layout_.size() / 2);
    }

    int compactor::get_file_length() const
    {
        auto length{0};

        for (size_t i = 0; i < layout_.size(); i += 2) {
            length += layout_[i];
        }

        return length;
    }

    int compactor::read()
    {
        auto result{0};

        if (index_ % 2 == 0) {
            result = front_.index / 2;
            front_.counter--;
            if (front_.counter == 0) {
                front_.index += 2;
                front_.counter = layout_[front_.index];
                index_++;
                free_space_ = layout_[index_];
                if (free_space_ == 0) {
                    index_++;
                }
            }
        }
        else {
            result = back_.index / 2;
            back_.counter--;
            if (back_.counter == 0) {
                back_.index -= 2;
                back_.counter = layout_[back_.index];
            }
            free_space_--;
            if (free_space_ == 0) {
                index_++;
            }
        }

        return result;
    }

    part2_data parse2(const std::string& row) {
        part2_data result {
            std::vector<block2>{},
			std::vector<queue_t>(10)
        };

        int index{0};
        for (size_t i = 0; i < row.size(); i++) {
			int length = row[i] - '0';
            if (i % 2) {
                if (length > 0) {
					result.queues[length].push(index);
                }
            }
            else {
                auto id = static_cast<int>(i / 2);
                result.blocks.push_back({index, id, length});
            }
            index += length;
        }

        return result;
    }
}
