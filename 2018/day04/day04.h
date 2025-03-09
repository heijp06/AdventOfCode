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
    private:
        std::map<std::string, std::vector<bool>> dates_;
        void change_sleep_state(const std::string& date, int minute, bool state);
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);

    std::string start_at_midnight(const std::string& line);
}
