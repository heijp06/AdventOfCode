#include "day04.h"

namespace day04 {
	int part1(const std::vector<std::string>& rows) {
		xmas_counter counter;
		int width = rows.at(0).size();
		int height = rows.size();

		for (int row = 0; row < height; row++) {
			const auto& line = rows.at(row);
			for (int column = 0; column < width; column++) {
				counter.add_char(line.at(column));
			}
			counter.new_line();
		}

		for (int row = 0; row < height; row++) {
			const auto& line = rows.at(row);
			for (int column = width - 1; column >= 0; column--) {
				counter.add_char(line.at(column));
			}
			counter.new_line();
		}

		for (int column = 0; column < width; column++) {
			for (int row = 0; row < height; row++) {
				const auto& line = rows.at(row);
				counter.add_char(line.at(column));
			}
			counter.new_line();
		}

		for (int column = 0; column < width; column++) {
			for (int row = height - 1; row >= 0; row--) {
				const auto& line = rows.at(row);
				counter.add_char(line.at(column));
			}
			counter.new_line();
		}

		for (int column_plus_row = 0; column_plus_row < width + height - 1; column_plus_row++) {
			for (int column = 0; column < width; column++) {
				int row = column_plus_row - column;
				if (row >= 0 && row < height) {
					const auto& line = rows.at(row);
					counter.add_char(line.at(column));
				}
			}
			counter.new_line();
		}

		for (int column_plus_row = 0; column_plus_row < width + height - 1; column_plus_row++) {
			for (int column = width - 1; column >= 0; column--) {
				int row = column_plus_row - column;
				if (row >= 0 && row < height) {
					const auto& line = rows.at(row);
					counter.add_char(line.at(column));
				}
			}
			counter.new_line();
		}

		for (int column_min_row = 1 - height; column_min_row < width; column_min_row++) {
			for (int column = 0; column < width; column++) {
				int row = column - column_min_row;
				if (row >= 0 && row < height) {
					const auto& line = rows.at(row);
					counter.add_char(line.at(column));
				}
			}
			counter.new_line();
		}

		for (int column_min_row = 1 - height; column_min_row < width; column_min_row++) {
			for (int column = width - 1; column >= 0; column--) {
				int row = column - column_min_row;
				if (row >= 0 && row < height) {
					const auto& line = rows.at(row);
					counter.add_char(line.at(column));
				}
			}
			counter.new_line();
		}

		return counter.get_count();
	}

	int part2(const std::vector<std::string>& rows) {
		auto count{ 0 };
		int width = rows.at(0).size();
		int height = rows.size();

		for (int row = 1; row < height - 1; row++) {
			for (int column = 1; column < width - 1; column++) {
				if (check_char(rows, 'A', row, column)) {
					if ((check_char(rows, 'M', row - 1, column - 1) && check_char(rows, 'S', row + 1, column + 1)) ||
						(check_char(rows, 'S', row - 1, column - 1) && check_char(rows, 'M', row + 1, column + 1))) {
						if ((check_char(rows, 'M', row - 1, column + 1) && check_char(rows, 'S', row + 1, column - 1)) ||
							(check_char(rows, 'S', row - 1, column + 1) && check_char(rows, 'M', row + 1, column - 1))) {
							count++;
						}
					}
				}
			}
		}

		return count;
	}

	bool check_char(const std::vector<std::string>& rows, char c, int row, int column) {
		return rows.at(row).at(column) == c;
	}

	xmas_counter::xmas_counter() : current_{}, count_{ 0 } {}

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
