#include <array>

#include "day14.h"
#include <iostream>

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

        while (true) {
            std::vector<robot> new_robots;

            auto q1{0};
            auto q2{0};
            auto q3{0};
            auto q4{0};
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

                if (x < (width - 1) / 2) {
                    q1 += y < (height - 1) / 2;
                    q3 += y > (height - 1) / 2;
                }
                else if (x > (width - 1) / 2) {
                    q2 += y < (height - 1) / 2;
                    q4 += y > (height - 1) / 2;
                }
                
                new_robots.push_back({{y, x}, {vy, vx}});
            }
            //if (q1 * q2 * q3 * q4 == 228410028) {
            //    std::cout << q1 << " " << q2 << " " << q3 << " " << q4 << std::endl;;
            //}

            robots = new_robots;

            output(robots, width, height);
        }

        return 0;
    }

    void output(const std::vector<robot>& robots, int width, int height) {
        static std::array<std::array<bool, 101>, 103> where;
        static auto counter{0};

        for (const auto& robot : robots) {
            where[robot.position.row][robot.position.column] = true;
        }

        //auto most{0};
        //for (const auto& robot : robots) {
        //    most += where[robot.position.row][robot.position.column] && where[robot.position.row][width - 1 - robot.position.column];
        //}
        //if (most < robots.size() / 2) {
        //    for (const auto& robot : robots) {
        //        where[robot.position.row][robot.position.column] = false;
        //    }
        //    counter++;
        //    if (counter % 1000 == 0) {
        //        std::cout << counter << " " << most << std::endl;
        //    }
        //    return;
        //}

        std::cout << std::endl;
        std::cout << "-----------------------------------------------------------------------------------------------------" << std::endl;
        std::cout << counter++ << std::endl;
        std::cout << "-----------------------------------------------------------------------------------------------------" << std::endl;
        std::cout << std::endl;

        for (int row = 0; row < height; row++) {
            for (int column = 0; column < width; column++) {
                std::cout << (where[row][column] ? "#" : " ");
            }
            std::cout << std::endl;
        }

        std::cout << std::endl;

        for (const auto& robot : robots) {
            where[robot.position.row][robot.position.column] = false;
        }

        //std::string s;
        //std::cin >> s;
    }
}
