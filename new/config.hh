#ifndef CONFIG_HH
#define CONFIG_HH

#include <cassert>
#include <cstddef>
#include <format>
#include <fstream>
#include <iostream>
#include <string>
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

*/

enum State { No = 0, Some = 1, Yes = 2 };
class Config;

// error //

template <class... Args>
void error(std::format_string<Args...> fmt, Args&&...args) {
    std::cerr << std::format(fmt, args...) << std::endl;
    exit(1);
}

// Base functions, which will be specialized for each type

class ConfigLines;
struct OptionInfo {
    size_t      id;
    const char *name;
    void (*read)(ConfigLines& lines);
};

template <size_t ID>
std::vector<OptionInfo> children() {
    return {};
}

template <size_t ID>
size_t level() {
    return size_t(-1);
}

template <size_t ID>
const char *name() {
    return "ERROR";
}

#define OPTION(option, level, id) \
    template <>                   \
    size_t level<id>() {          \
        return size_t(level);     \
    }                             \
    template <>                   \
    const char *name<id>() {      \
        return #option;           \
    }

#define OPTION_SET(option, level, id) \
    template <>                                    \
    size_t level<id>() {                           \
        return size_t(level);                      \
    }                                              \
    template <>                                    \
    const char *name<id>() {                       \
        return #option;                            \
    }                                              \
    template <>                                    \
    std::vector<OptionInfo> children() {           \
        return {};                                 \
    }

// ConfigLines //

// Object that reads the configuration and stores individual lines
// ```
// strings: yes           -> Line(0, "strings", Yes)
//     access_chars: no   -> Line(1, "access_chars", No)
//     substr: no         -> Line(2, "substr", No)
// ```

class ConfigLines {
   public:
    struct Line {
        size_t      level;
        std::string name;
        State       state;
    };

   private:
    std::ifstream file_;

    Line config_line_;
    int  lin_ = 0, col_;

    const Line& curr() const { return config_line_; }

    Line parse_line(std::string line);

   public:
    ConfigLines(std::string filename);

    int lin() const { return lin_; }

    size_t             level() const { return curr().level; }
    const std::string& name() const { return curr().name; }
    State              state() const { return curr().state; }

    bool finished() const { return not bool(file_); }

    void next();
};

class Config {
   private:
    std::vector<State> states_;

    static constexpr size_t max_id_ = 9;

    template <typename T>
    void read(ConfigLines& lines) {
        assert(lines.name() == name<T>());
        lines.next();
        set(T::id, lines.state());
        while (lines.level() > level<T>()) {
            if (lines.level() != level<T>() + 1) {
                error(".minicc:{}:0: Wrong indentation level.", lines.lin());
            }
            bool processed = false;
            for (OptionInfo info : children<T>()) {
                if (lines.name() == info.name) {
                    processed = true;
                    (*info.read)(lines);
                }
            }
            if (not processed) {
                error(".minicc:{}:0: Unexpected option '{}'.", lines.lin(), lines.name());
            }
        }
    }

   public:
    State get(size_t id) { return states_[id]; }

    void set(size_t id, State value) {
        if (id >= states_.size()) {
            states_.resize(id + 1, No);
        }
        assert(value == Yes || value == No);
        states_[id] = value;
    }

    template <typename T>
    State get() {
        return get(T::id);
    }

    template <typename T>
    void set(State value) {
        set(T::id, value);
    }

    void read() { ConfigLines config_lines(".minicc"); }
};

#endif
