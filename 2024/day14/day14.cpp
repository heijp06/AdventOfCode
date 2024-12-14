#include <iostream>
#include <set>

#include "day14.h"

namespace day14 {
    int part1(const std::vector<std::string>& rows, int width, int height) {
        int q1 = 0;
        int q2 = 0;
        int q3 = 0;
        int q4 = 0;

        for (const auto& row : rows) {
            const auto& data = advent::ints(row);
            auto x0 = data[0];
            auto y0 = data[1];
            auto vx = data[2];
            auto vy = data[3];

            auto x = (x0 + vx * 100) % width;
            if (x < 0) {
                x += width;
            }
            auto y = (y0 + vy * 100) % height;
            if (y < 0) {
                y += height;
            }

            if (x < (width - 1) / 2) {
                q1 += y < (height - 1) / 2;
                q3 += y > (height - 1) / 2;
            }
            else if (x > (width - 1) / 2) {
                q2 += y < (height - 1) / 2;
                q4 += y > (height - 1) / 2;
            }
        }

        return q1 * q2 * q3 * q4;
    }

    int part2(const std::vector<std::string>& rows, int width, int height) {
        std::vector<robot> robots;

        for (const auto& row : rows) {
            const auto& data = advent::ints(row);
            auto x0 = data[0];
            auto y0 = data[1];
            auto vx = data[2];
            auto vy = data[3];

            robots.push_back({{y0, x0}, {vy, vx}});
        }

        output(robots, width, height);
        auto min_variance{variance(robots)};
        auto t_min{0};

        for (int t = 1; t <= width * height; t++) {
            std::vector<robot> new_robots;

            for (const auto& robot : robots) {
                auto x0 = robot.position.column;
                auto y0 = robot.position.row;
                auto vx = robot.velocity.column;
                auto vy = robot.velocity.row;

                auto x = (x0 + vx) % width;
                if (x < 0) {
                    x += width;
                }
                auto y = (y0 + vy) % height;
                if (y < 0) {
                    y += height;
                }

                new_robots.push_back({{y, x}, {vy, vx}});
            }

            robots = new_robots;

            auto var{variance(robots)};
            if (var < min_variance) {
                std::cout << std::endl;
                std::cout << "-----------------------------------------------------------------------------------------------------" << std::endl;
                std::cout << t << std::endl;
                std::cout << "-----------------------------------------------------------------------------------------------------" << std::endl;
                std::cout << std::endl;

                output(robots, width, height);
                min_variance = var;
                t_min = t;
            }
        }

        return t_min;
    }

    void output(const std::vector<robot>& robots, int width, int height) {
        std::set<advent::coord> where;

        for (const auto& robot : robots) {
            where.insert(robot.position);
        }

        for (int row = 0; row < height; row++) {
            for (int column = 0; column < width; column++) {
                std::cout << (where.count({row, column}) ? "#" : " ");
            }
            std::cout << std::endl;
        }
    }

    int variance(const std::vector<robot>& robots) {
        auto sum{0};
        auto avg = average(robots);

        for (const auto& robot : robots) {
            auto delta = robot.position - avg;
            sum += delta.row * delta.row + delta.column * delta.column;
        }

        return sum;
    }

    advent::coord average(const std::vector<robot>& robots) {
        advent::coord sum{};

        for (const auto& robot : robots) {
            sum = sum + robot.position;
        }

        auto n = static_cast<int>(robots.size());

        return {sum.row / n, sum.column / n};
    }
}
