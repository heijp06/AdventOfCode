#pragma once

#include <cstdint>
#include <string>
#include <vector>

namespace day17 {
    using data_t = std::vector<std::int64_t>;

    class computer {
    public:
        computer(std::int64_t a, std::int64_t b, std::int64_t c, const data_t& program);
        std::int64_t a() const;
        std::int64_t b() const;
        std::int64_t c() const;
        data_t run();
        const data_t program() const;
    private:
        std::int64_t a_;
        std::int64_t b_;
        std::int64_t c_;
        const data_t program_;
        std::int64_t instruction_pointer_;
        data_t output_;
        std::int64_t combo();
    };

    std::string part1(const std::vector<std::string>& rows);
    std::int64_t part2(const std::vector<std::string>& rows);

    computer parse(const std::vector<std::string>& rows);
}
