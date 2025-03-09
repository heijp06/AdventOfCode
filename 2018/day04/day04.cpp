#include <algorithm>
#include <iterator>

#include "day04.h"
#include "../../lib/advent.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows) {
        const auto& guards = parse(rows);

        return -1;
    }

    std::map<std::string, guard> parse(const std::vector<std::string>& rows) {
        std::vector<std::string> records;

        std::transform(rows.cbegin(), rows.cend(), std::back_inserter(records), start_at_midnight);
        std::sort(records.begin(), records.end());

        std::map<std::string, guard> guards;
        std::string id;

        for (const auto& record : records) {
            const auto& date = record.substr(6, 5);
            const auto& minute = std::stoi(record.substr(15, 2));
            switch (record.at(19)) {
            case 'G': // Guard #99 begins shift
                id = advent::split(record, " ").at(3);
                guards[id].add_date(date);
                break;
            case 'f': // falls asleep
                guards[id].sleep(date, minute);
                break;
            case 'w': // wakes up
                guards[id].wake(date, minute);
                break;
            }
        }

        return guards;
    }

    int part2(const std::vector<std::string>& rows) {
        (void)rows;
        return -1;
    }

    std::string start_at_midnight(const std::string& line) {
        if (line.at(12) != '2') {
            return line;
        }

        auto ones_digit = line.at(10);

        if (ones_digit == '9') {
            auto tens_digit = line.at(9);
            return line.substr(0, 9) + std::string(1, tens_digit + 1) + std::string("0 00:00") + line.substr(17);
        }

        return line.substr(0, 10) + std::string(1, ones_digit + 1) + std::string(" 00:00") + line.substr(17);
    }

    void guard::add_date(const std::string& date) {
        sleep_state_[date] = std::vector<bool>(60);
    }

    void guard::sleep(const std::string& date, int minute) {
        change_sleep_state(date, minute, true);
    }

    void guard::wake(const std::string& date, int minute) {
        change_sleep_state(date, minute, false);
    }

    void guard::change_sleep_state(const std::string& date, int minute, bool state) {
        auto& sleep_state = sleep_state_[date];

        for (size_t i = minute; i < sleep_state.size(); i++) {
            sleep_state[i] = state;
        }
    }
}
