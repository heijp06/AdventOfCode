#pragma once

#include <map>
#include <string>
#include <vector>

namespace day04 {
    class guard {
    public:
        void add_date(const std::string& date);
        void sleep(const std::string& date, int minute);
        void wake(const std::string& date, int minute);
        int minutes_asleep() const;
        int asleep_most() const;
    private:
        std::map<std::string, std::vector<bool>> sleep_state_;
        void change_sleep_state(const std::string& date, int minute, bool state);
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::map<std::string, guard> parse(const std::vector<std::string>& rows);
    std::string start_at_midnight(const std::string& line);
    std::string format(int number);
}
