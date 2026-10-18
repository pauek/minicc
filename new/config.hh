#ifndef CONFIG_HH
#define CONFIG_HH

#include <cstddef>
#include <string>
#include <vector>

//
// - Config::get should be O(1), therefore:
// 	 1. We must read the config and store it in a vector<State> (State = { No, Some, Yes }),
//      already precomputed.
//   2. The call to Config::get needs compile-time translation from type to ID.
//

// Interface:
// - Get config: `Config::get<Config::strings::substr>() -> State`
// - Change config at runtime: `Config::set<Config::strings::substr>(true)` (+ recomputes State).
// - Read config: `Config::read()`. Reads from file.

enum State { No = 0, Some = 1, Yes = 2 };

class Config;

template <size_t ID>
struct Option {
    static constexpr size_t id = ID;

    void set(Config& config, bool value);
};

template <size_t ID, size_t MinChildId, size_t MaxChildId>
struct OptionSet {
    static constexpr size_t min_child_id = MinChildId;
    static constexpr size_t max_child_id = MaxChildId;
};

#define NAME(__name__) static constexpr const char *name = #__name__
#define OPTION(__id__, __name__)       \
    struct __name__ : Option<__id__> { \
        NAME(__name__);                \
    }

class Config {
   public:
    struct strings : OptionSet<1, 2, 4> {
        NAME("strings");
        OPTION(2, size);
        OPTION(3, char_access);
        OPTION(4, substr);
    };
    struct bit_ops : OptionSet<5, 6, 9> {
        NAME("bit_ops");
        OPTION(6, bit_and);
        OPTION(7, bit_or);
        OPTION(8, bit_xor);
        OPTION(9, bit_not);
    };

   private:
    static constexpr size_t max_id_ = 9;

    std::vector<State> states_;

    static Config global_config_;

    void read_from_file_(std::string filename);

   public:
    Config() : states_(max_id_, No) {
    	read_from_file_(".minicc");
    }

    void set(size_t id, bool value) { states_[id] = value ? Yes : No; }

    State get(size_t id) { return states_[id]; }

    template <typename T>
    State get() {
        return get(T::id);
    }
};

template <size_t ID>
void Option<ID>::set(Config& config, bool value) {
    config.set(ID, value);
}

#endif
