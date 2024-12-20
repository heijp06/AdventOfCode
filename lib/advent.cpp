#include "advent.h"

#include <filesystem>
#include <fstream>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <iostream>

namespace advent {
    // Read the input file for @year and @day as a vector of strings
    std::vector<std::string> get_rows(int year, int day) {
        auto path{get_data_path(year, day)};
        std::vector<std::string> rows{};
        std::ifstream input{path};

        for (std::string row; getline(input, row); ) {
            // Read files with Windows line endings correctly on Linux.
            if (!row.empty() && row.back() == '\r') {
                row.erase(row.length() - 1);
            }
            rows.push_back(row);
        }

        return rows;
    }

    // Get the path to the input file for @year and @day
    std::string get_data_path(int year, int day) {
        const std::string data_folder{"data"};
        auto year_folder{std::to_string(year)};
        std::ostringstream ss;
        ss << "day" << std::setw(2) << std::setfill('0') << day << ".txt";
        std::string day_file(ss.str());

        auto current{std::filesystem::current_path()};
        while (!std::filesystem::exists(current / data_folder)) {
            if (!current.has_parent_path()) {
                throw std::domain_error("Cannot find " + data_folder + " folder.");
            }
            current = current.parent_path();
        }

        auto input = current / data_folder / year_folder / day_file;

        if (!std::filesystem::exists(input)) {
            throw std::domain_error(input.string() + " does not exist.");
        }

        return input.string();
    }

    // Get all the integers from a string.
    std::vector<int> ints(const std::string& row) {
        return ints<int>(row);
    }

    std::vector<std::string> split(const std::string& text, const std::string& delimiter) {
        std::vector<std::string> result;
        size_t pos{0};

        while (pos < text.size()) {
            auto new_pos = delimiter.empty() ? pos + 1 : text.find(delimiter, pos);
            if (new_pos == std::string::npos) {
                new_pos = text.size();
            }
            result.push_back(text.substr(pos, new_pos - pos));
            pos = new_pos + delimiter.size();
        }

        return result;
    }

    grid::grid(const std::vector<std::string>& rows) :
        rows_{rows},
        height_{static_cast<int>(rows.size())},
        width_{static_cast<int>(height_ ? rows[0].size() : 0)} {
    }

    int grid::get_height() const {
        return height_;
    }

    int grid::get_width() const {
        return width_;
    }

    std::vector<coord> grid::get_positions() const {
        std::vector<coord> result;

        for (int row = 0; row < height_; row++) {
            for (int column = 0; column < width_; column++) {
                result.push_back({row, column});
            }
        }

        return result;
    }

    advent::coord grid::find(const char c) const {
        for (const auto& position : get_positions()) {
            if (rows_[position.row][position.column] == c) {
                return position;
            }
        }

        throw std::domain_error("Cannot find: " + c);
    }

    std::vector<advent::coord> grid::find_all(const char c) const {
        std::vector<advent::coord> result;

        for (const auto& position : get_positions()) {
            if (rows_[position.row][position.column] == c) {
                result.push_back(position);
            }
        }

        return result;
    }

    void grid::draw() const {
        for (int row = 0; row < height_; row++) {
            for (int column = 0; column < width_; column++) {
                std::cout << rows_[row][column];
            }
            std::cout << std::endl;
        }
    }

    direction direction::turn_left() const {
        return { -delta_column, delta_row };
    }

    direction direction::turn_right() const {
        return { delta_column, -delta_row };
    }

    const direction direction::up() {
        return {-1, 0};
    }

    const direction direction::down() {
        return {1, 0};
    }

    const direction direction::left() {
        return {0, -1};
    }

    const direction direction::right() {
        return {0, 1};
    }

    const direction direction::north() {
        return up();
    }

    const direction direction::south() {
        return down();
    }

    const direction direction::east() {
        return right();
    }

    const direction direction::west() {
        return left();
    }

    const std::vector<direction> direction::nsew() {
        return std::vector<direction>{north(), south(), east(), west()};
    }
}
