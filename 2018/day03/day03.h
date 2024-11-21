#pragma once

#include <string>
#include <vector>

namespace day03 {
    struct claim {
        int id;
        int left;
        int top;
        int width;
        int height;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
    claim parse(std::string row);

    class QuadTree {
    public:
        virtual ~QuadTree() {};
        virtual int size() = 0;
        virtual void insert(claim claim) = 0;
    };

    class Leaf : public QuadTree {
    public:
        Leaf(int size, int overlap);
        int size() override;
        void insert(claim claim) override;
    private:
        int size_;
        int overlap_;
    };
}
