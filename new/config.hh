#ifndef CONFIG_HH
#define CONFIG_HH

#include <cassert>
#include <cstdint>
#include <iostream>
#include <vector>

/*

Configuration
-------------

Config::get should be O(1), therefore:
1. We must read the config and store it in a vector<State>, precomputed.
2. The call to Config::get needs compile-time translation from type to ID.

Also:
- We need to be able to: a) show the config, b) show all the options.
- Therefore, walk the tree of options.

Interface:

`GET<a.b.c> () --> State`
    Get config.

`SET <a.b.c> (<Yes | No>)`
    Change config at runtime, and recompute State for parents = walk tree upwards.

`READ_CONFIG()`
    Read config from file. Needs to search by name, hierarchically.

`SHOW_OPTION_LIST()`
    Show all options.

`SHOW_CONFIG()`
    Show the current config.

ConfigTree
----------

A tree stored in levels. Each node has its children consecutive in the next level.

Fields for each level:
- `vector<const char *> names`.
- `vector<range> children` (in the next level).
- `vector<uint16_t> parents` (in the previous level).

*/

namespace Config {

enum State { No = 0, Some = 1, Yes = 2 };

inline const char *state_to_str(State state) {
    switch (state) {
        case No:
            return "no";
        case Some:
            return "some";
        case Yes:
            return "yes";
    }
}

struct ID {                  // 32 bit
    unsigned int level : 4;  // 16 levels maximum (more than enough!)
    unsigned int index : 28;

    ID(uint8_t level_, uint32_t index_) {
        assert(level_ < 16);
        assert(index_ < 0x1000000);
        level = level_;
        index = index_;
    }

    bool is_all() { return level == 0 && index == 0; }
};

struct Range {
    int  begin = 0, end = 0;
    bool empty() const { return begin >= end; }
};


// Access to configuration by name a.b.c (typed)

template <uint8_t level, uint32_t index>
struct Option {
	static ID id() { return ID(level, index); }
};

template <uint8_t level, uint32_t index>
const Option<level, index>::ID id(level, index);

struct strings : Option<1, 0> {
	struct substr : Option<2, 0> {};
	struct char_access : Option<2, 1> {};
};
struct bit_ops : Option<1, 1> {
	struct bit_and : Option<2, 2> {};
	struct bit_or : Option<2, 3> {};
	struct bit_xor : Option<2, 4> {};
};

// Config levels

struct Level {
    std::vector<const char *> names;
    std::vector<Range>        children;  // in the upper level
    std::vector<int>          parent;    // in the lower level
    std::vector<State>        state;     // State of each option

    size_t size() const { return names.size(); }
};

void read_config(std::istream& in, ID id);

extern std::vector<Level> levels;

void read_from_file();

inline std::string get_name(ID id) {
    return levels[id.level].names[id.index];
}

inline ID parent(ID id) {
    assert(id.level < levels.size());
    assert(id.index < levels[id.level].state.size());
    int i = levels[id.level].parent[id.index];
    return ID(id.level - 1, i);
}

inline Range children(ID id) {
    assert(id.level < levels.size());
    if (id.index >= levels[id.level].children.size()) {
        return {0, 0};
    }
    return levels[id.level].children[id.index];
}

template <typename Opt>
inline State get() {
    return get(Opt::id());
}

inline State get(ID id) {
    assert(id.level < levels.size());
    assert(id.index < levels[id.level].state.size());
    return levels[id.level].state[id.index];
}

void set(ID id, State new_state);

void show();
void show_all_options();

void recompute_all();

}  // namespace Config

#endif
