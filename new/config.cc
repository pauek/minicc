#include <format>
#include <fstream>
#include <iostream>
using namespace std;

#include "config.hh"

namespace Config {

// utils //

template <class... Args>
void error(std::format_string<Args...> fmt, Args&&...args) {
    cerr << format(fmt, args...) << endl;
    exit(1);
}

bool allowed_char(char c) {
    return isalpha(c) || c == '_';
}

bool all_spaces(std::string line) {
    for (char c : line) {
        if (not isspace(c)) {
            return false;
        }
    }
    return true;
}

struct NameLevel {
    string  name;
    uint8_t level;
};
NameLevel split_config_line(string line, int lin) {
    size_t col = 1;
    while (col <= line.size() && isspace(line[col - 1])) {
        col++;
    }
    int indent = col;
    if (indent % 4 != 0) {
        error(".minicc:{}:{}: Indentation levels have to be multiples of 4 spaces!", lin, col);
    }

    NameLevel result = {
        .name = "",
        .level = uint8_t(indent / 4),
    };
    while (col < line.size()) {
        char ch = line[col - 1];
        if (isspace(ch)) {
            error(".minicc:{}:{}: Spaces are not allowed config names.", lin, col);
        }
        if (not allowed_char(ch)) {
            error(".minicc:{}:{}: Character '{}' not allowed in config names.", lin, col, ch);
        }
        result.name += ch;
        col++;
    }

    return result;
}

//

/*
strings
        substr
        char_access
bit_ops
        bit_and
        bit_or
*/

std::vector<Level> levels = {
    {{"all"},                                                   {{0, 2}},         {-1},   {No}    },
    {{"strings", "bit_ops"},                                    {{0, 2}, {2, 5}}, {0, 0}, {No, No}},
    {{"substr", "char_access", "bit_and", "bit_or", "bit_xor"},
     {},
     {0, 0, 1, 1},
     {Yes, No, Yes, No, Yes}                                                                           }
};

//

struct Input {
    std::istream& stream;

    int lin, col;

    std::string get_line() {
        string line;
        getline(stream, line);
        lin++;
        return line;
    }
};

void read_config(Input input, ID id) {
    while (true) {
        string line = input.get_line();
        auto [name, level] = split_config_line(line, input.lin);
        if (level > id.level + 1) {
            error(".minicc:{}:0: Wrong indentation level.", input.lin);
        }
    }
}

void read_from_file() {
    ifstream fin(".minicc");
    read_config({.stream = fin, .lin = 1, .col = 1}, ID{0, 0});
}

// Show all options

struct OptionDumper {
    ostream& out;
    OptionDumper(ostream& out_) : out(out_) {}

    void show_option(ID id) {
        string indent(id.level * 4, ' ');
        out << indent << get_name(id) << endl;
        auto ch = children(id);
        for (auto i = ch.begin; i < ch.end; i++) {
            show_option(ID(id.level + 1, i));
        }
    }
};

void show_all_options() {
    OptionDumper(cout).show_option({0, 0});
}

// Show the configuration

struct ConfigDumper {
    ostream& out;
    ConfigDumper(ostream& out_) : out(out_) {}

    void show_config(ID id) {
        string indent(id.level * 4, ' ');
        auto   state = get(id);
        if (state != No) {
            out << indent << get_name(id) << ": " << (state == Yes ? "yes" : "") << endl;
            auto ch = children(id);
            if (not ch.empty() && get(id) == Some) {
                for (auto i = ch.begin; i < ch.end; i++) {
                    show_config(ID(id.level + 1, i));
                }
            }
        }
    }
};

void show() {
    ConfigDumper(cout).show_config(ID(0, 0));
}

// Set an option

void recompute_state(ID id) {
    auto ch = children(id);
    bool all = true, none = true;
    for (auto i = ch.begin; i < ch.end; i++) {
        auto child_id = ID(id.level + 1, i);
        auto child_state = get(child_id);
        if (child_state == Some) {
            none = false;
            all = false;
        } else if (child_state == Yes) {
            none = false;
        } else if (child_state == No) {
            all = false;
        }
    }
    State new_state = Some;
    if (all) {
        new_state = Yes;
    } else if (none) {
        new_state = No;
    }
    levels[id.level].state[id.index] = new_state;
}

void recompute_all() {
    for (int i = levels.size() - 2; i >= 0; i--) {
        for (int j = 0; j < levels[i].size(); j++) {
            recompute_state(ID(i, j));
        }
    }
}

// Propagate to parents (recursively), to recompute affected options
void propagate_up(ID id) {
    if (id.is_all()) {
        return;
    }
    auto parent_id = parent(id);
    recompute_state(parent_id);
    propagate_up(parent_id);
}

void propagate_down(ID id, State new_state) {
    // If it has children, propagate to them
    auto ch = children(id);
    if (not ch.empty()) {
        for (auto i = ch.begin; i < ch.end; i++) {
            auto child_id = ID(id.level + 1, i);
            set(child_id, new_state);
            propagate_down(child_id, new_state);
        }
    }
}

void set(ID id, State new_state) {
    assert(new_state != Some);  // It does not make sense to set a state such as "Some"
    assert(id.level < levels.size());
    assert(id.index < levels[id.level].state.size());

    // Set state for this id
    levels[id.level].state[id.index] = new_state;

    // Propagate up & down
    propagate_down(id, new_state);
    propagate_up(id);
}

}  // namespace Config
