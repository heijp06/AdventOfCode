#include <string>
#include <vector>

namespace day03 {
    class claim {
    public:
        claim(int id, int left, int top, int width, int height);
        int id() const;
        int left() const;
        int top() const;
        int width() const;
        int height() const;
    private:
        int id_;
        int left_;
        int top_;
        int width_;
        int height_;
    };

    int part1(const std::vector<std::string>& rows);
    int part2(const std::vector<std::string>& rows);
    claim parse(std::string row);
}
