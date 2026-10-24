#ifndef CONFIG_CONFIG_HH
#define CONFIG_CONFIG_HH

#include <cstdint>
#include <cassert>

namespace Config {

enum State { None = -1, No = 0, Some = 1, Yes = 2 };

inline const char *state_to_str(State state) {
    switch (state) {
        case No:
            return "no";
        case Some:
            return "some";
        case Yes:
            return "yes";
        case None:
            return "";
    }
}

struct ID {         // 32 bit
    int level : 4;  // 16 levels maximum (more than enough!)
    int index : 28;

    ID() : level(-1), index(-1) {}

    ID(uint8_t level_, uint32_t index_) {
        assert(level_ < 16);
        assert(index_ < 0x1000000);
        level = level_;
        index = index_;
    }

    static ID none;

    bool operator==(const ID& other) const { return level == other.level && index == other.index; }
    bool operator==(ID& other) { return level == other.level && index == other.index; }

    bool is_all() { return level == 0 && index == 0; }
};

}  // namespace Config

#endif
