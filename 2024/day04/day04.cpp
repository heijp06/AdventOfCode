#include "day04.h"
#include "../../lib/advent.h"

namespace day04 {
    int part1(const std::vector<std::string>& rows) {
        xmas_counter counter;
        const auto grid = advent::grid(rows);

        for (int row = 0; row < grid.get_height(); row++) {
            for (int column = 0; column < grid.get_width(); column++) {
                counter.add_char(grid[{row, column}]);
            }
            counter.new_line();
        }

        for (int row = 0; row < grid.get_height(); row++) {
            for (int column = grid.get_width() - 1; column >= 0; column--) {
                counter.add_char(grid[{row, column}]);
            }
            counter.new_line();
        }

        for (int column = 0; column < grid.get_width(); column++) {
            for (int row = 0; row < grid.get_height(); row++) {
                counter.add_char(grid[{row, column}]);
            }
            counter.new_line();
        }

        for (int column = 0; column < grid.get_width(); column++) {
            for (int row = grid.get_height() - 1; row >= 0; row--) {
                counter.add_char(grid[{row, column}]);
            }
            counter.new_line();
        }

        for (int column_plus_row = 0; column_plus_row < grid.get_width() + grid.get_height() - 1; column_plus_row++) {
            for (int column = 0; column < grid.get_width(); column++) {
                int row = column_plus_row - column;
                if (row >= 0 && row < grid.get_height()) {
                    counter.add_char(grid[{row, column}]);
                }
            }
            counter.new_line();
        }

        for (int column_plus_row = 0; column_plus_row < grid.get_width() + grid.get_height() - 1; column_plus_row++) {
            for (int column = grid.get_width() - 1; column >= 0; column--) {
                int row = column_plus_row - column;
                if (row >= 0 && row < grid.get_height()) {
                    counter.add_char(grid[{row, column}]);
                }
            }
            counter.new_line();
        }

        for (int column_min_row = 1 - grid.get_height(); column_min_row < grid.get_width(); column_min_row++) {
            for (int column = 0; column < grid.get_width(); column++) {
                int row = column - column_min_row;
                if (row >= 0 && row < grid.get_height()) {
                    counter.add_char(grid[{row, column}]);
                }
            }
            counter.new_line();
        }

        for (int column_min_row = 1 - grid.get_height(); column_min_row < grid.get_width(); column_min_row++) {
            for (int column = grid.get_width() - 1; column >= 0; column--) {
                int row = column - column_min_row;
                if (row >= 0 && row < grid.get_height()) {
                    counter.add_char(grid[{row, column}]);
                }
            }
            counter.new_line();
        }

        return counter.get_count();
    }

    int part2(const std::vector<std::string>& rows) {
        auto count{0};
        const auto grid = advent::grid(rows);

        for (int row = 1; row < grid.get_height() - 1; row++) {
            for (int column = 1; column < grid.get_width() - 1; column++) {
                if (grid[{row, column}] == 'A') {
                    if ((grid[{row - 1, column - 1}] == 'M' && grid[{row + 1, column + 1}] == 'S') ||
                        (grid[{row - 1, column - 1}] == 'S' && grid[{row + 1, column + 1}] == 'M')) {
                        if ((grid[{row - 1, column + 1}] == 'M' && grid[{row + 1, column - 1}] == 'S') ||
                            (grid[{row - 1, column + 1}] == 'S' && grid[{row + 1, column - 1}] == 'M')) {
                            count++;
                        }
                    }
                }
            }
        }

        return count;
    }

    xmas_counter::xmas_counter() : current_{}, count_{0} {}

    void xmas_counter::add_char(const char c) {
        switch (c) {
        case 'X':
            current_ = "X";
            break;
        case 'M':
            current_ == "X" ? current_ = "XM" : current_ = "";
            break;
        case 'A':
            current_ == "XM" ? current_ = "XMA" : current_ = "";
            break;
        case 'S':
            if (current_ == "XMA") {
                count_++;
            }
            current_ = "";
            break;
        }
    }

    void xmas_counter::new_line() {
        current_ = "";
    }

    int xmas_counter::get_count() {
        return count_;
    }
}
