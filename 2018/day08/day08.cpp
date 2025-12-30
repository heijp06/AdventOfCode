#include "day08.h"
#include "day08.h"
#include "../../lib/advent.h"

namespace day08 {
    int part1(const std::vector<std::string>& rows) {
        Reader reader{advent::ints(rows[0])};

        return sum_of_metadata(reader);
    }

    int part2(const std::vector<std::string>& rows) {
        Reader reader{advent::ints(rows[0])};

        return value(reader);
    }

    int sum_of_metadata(Reader& reader) {
        int sum{};
        int childs{reader.read()};
        int metadata{reader.read()};

        for (size_t i = 0; i < childs; i++) {
            sum += sum_of_metadata(reader);
        }

        for (size_t i = 0; i < metadata; i++) {
            sum += reader.read();
        }

        return sum;
    }

    int value(Reader& reader) {
        int sum{};
        int childs{reader.read()};
        int metadata{reader.read()};

        if (!childs) {
            for (size_t i = 0; i < metadata; i++) {
                sum += reader.read();
            }

            return sum;
        }

        std::vector<int> sums_of_childs{};
        sums_of_childs.reserve(childs);

        for (size_t i = 0; i < childs; i++) {
            sums_of_childs.push_back(value(reader));
        }

        for (size_t i = 0; i < metadata; i++) {
            int index{reader.read()};
            
            if (index >= 1 && index <= childs) {
                sum += sums_of_childs[index - 1];
            }
        }

        return sum;
    }

    Reader::Reader(const std::vector<int>& numbers) : numbers_{numbers}, pos_{0} {}

    int day08::Reader::read() {
        return numbers_[pos_++];
    }
}
