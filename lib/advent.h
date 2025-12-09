#pragma once

#include <cctype>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

namespace advent {
    std::vector<std::string> get_rows(int year, int day);
    std::string get_data_path(int year, int day);
    std::vector<int> ints(const std::string& row);
    std::vector<std::string> split(const std::string& text, const std::string& delimiter);

    // Get all the integers of type T from a string.
    template<typename T>
    std::vector<T> ints(const std::string& row) {
        std::vector<T> result{};
        T num{};
        bool negative{};
        bool innum{};

        for (const auto c : row) {
            if (std::isdigit(c)) {
                innum = true;
                num = 10 * num + c - '0';
                continue;
            }

            if (innum) {
                innum = false;
                result.push_back(negative ? -num : num);
                num = 0;
            }

            negative = c == '-';
        }

        if (innum) {
            result.push_back(negative ? -num : num);
        }

        return result;
    }

    struct direction {
        int delta_row;
        int delta_column;

        direction turn_left() const;
        direction turn_right() const;

        const static direction up();
        const static direction down();
        const static direction left();
        const static direction right();
        const static direction north();
        const static direction south();
        const static direction east();
        const static direction west();
        const static std::vector<direction> nsew();

        friend bool operator==(const direction& l, const direction& r) {
            return l.delta_row == r.delta_row && l.delta_column == r.delta_column;
        }
        friend bool operator<(const direction& l, const direction& r) {
            return l.delta_row == r.delta_row ? l.delta_column < r.delta_column : l.delta_row < r.delta_row;
        }
        friend bool operator>(const direction& l, const direction& r) {
            return l.delta_row == r.delta_row ? l.delta_column > r.delta_column : l.delta_row > r.delta_row;
        }
        friend direction operator*(const int times, const direction& dir) {
            return {dir.delta_row * times, dir.delta_column * times};
        }
    };

    struct coord {
        int row;
        int column;

        friend bool operator==(const coord& l, const coord& r) {
            return l.row == r.row && l.column == r.column;
        }
        friend bool operator!=(const coord& l, const coord& r) {
            return !(l == r);
        }
        friend bool operator<(const coord& l, const coord& r) {
            return l.row == r.row ? l.column < r.column : l.row < r.row;
        }
        friend bool operator>(const coord& l, const coord& r) {
            return l.row == r.row ? l.column > r.column : l.row > r.row;
        }
        friend coord operator+(const coord& l, const coord& r) {
            return {l.row + r.row, l.column + r.column};
        }
        friend coord operator+(const coord& l, const direction& r) {
            return {l.row + r.delta_row, l.column + r.delta_column};
        }
        friend coord& operator+=(coord& position, const direction& rhs) {
            position = position + rhs;
            return position;
        }
        friend coord operator-(const coord& l, const coord& r) {
            return {l.row - r.row, l.column - r.column};
        }
        friend coord operator-(const coord& c) {
            return {-c.row, -c.column};
        }
    };

    class grid {
    public:
        grid(const std::vector<std::string>& rows);
        grid(int height, int width);
        int get_height() const;
        int get_width() const;
        std::vector<coord> get_positions() const;
        advent::coord find(const char c) const;
        std::vector<advent::coord> find_all(const char c) const;
        void draw() const;

        char& operator[](const advent::coord& index) {
            return rows_[index.row][index.column];
        }
        const char& operator[](const advent::coord& index) const {
            return rows_[index.row][index.column];
        }
    private:
        std::vector<std::string> rows_;
        const int height_;
        const int width_;
    };

    template<typename Iterator>
    std::string join(Iterator begin, Iterator end, std::string delimiter) {
        std::string result;

        for (auto& it = begin; it != end; ++it) {
            if (result.size()) {
                result += delimiter;
            }
            result += *it;
        }

        return result;
    }
}
