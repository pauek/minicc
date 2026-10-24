#include <format>
#include <fstream>
#include <iostream>
#include <stack>
using namespace std;

#include "config.hh"
#include "utils.hh"
#include "config/config-line.hh"

namespace Config {

ID ID::none;

#include "generated/config-levels.cc"

//

constexpr const char *CONFIG_FILE = ".minicc";

struct ConfigReader {
    std::istream& stream;
    int           lin, col;

   public:
    ConfigReader(istream& in) : stream(in), lin(0), col(1) {}

    bool get_line(ConfigLine& config_line) {
        string line;
        bool   ok = bool(getline(stream, line));
        if (not ok) {
            return false;
        }
        lin++;
        config_line = split_config_line(line, lin);
        return true;
    }

    void read_config(ID start_id = ID()) {
        // The lines read in this loop have to be children of ID
        // in the case of ID.is_none() the child is `all`.

        stack<ID> stack;
        stack.push(start_id);

        while (true) {
            ID id = stack.top();
            if (stream.eof()) {
                break;
            }
            // Parse the line first
            ConfigLine config_line;
            if (not get_line(config_line)) {
            	break;
            }

            // If parsing gave an error, show it
            if (config_line.error.present) {
                const auto& e = config_line.error;
                error("{}:{}:{}: {}", CONFIG_FILE, e.lin, e.col, e.message);
                break;
            }

            // If the level is lower, unwind the stack
            while (stack.top().level >= config_line.level) {
                stack.pop();
            }
            // Replace the current id
            id = stack.top();

            // Check that the level is not too deep
            if ((id == ID::none && config_line.level != 0) || (config_line.level != id.level + 1)) {
                int indent = config_line.level * 4 + 1;
                error("{}:{}:{}: Unexpected indentation level", CONFIG_FILE, lin, indent);
                break;
            }
            auto child_id = find_child(id, config_line.name);
            if (child_id == ID::none) {
                auto name = get_name(id);
                int indent = config_line.level * 4;
                string message = std::format("\"{}\" not found in \"{}\"", config_line.name, name);
                if (id == ID::none) {
                    message = std::format("\"{}\" not found at top-level", config_line.name);
                }
                error("{}:{}:{}: {}", CONFIG_FILE, lin, indent, message);
                break;
            }

            // If there is a value, set it
            if (config_line.value != State::None) {
                set(child_id, config_line.value);
            }

            stack.push(child_id);
        }
    }
};

void read_from_file() {
    ID all(0, 0);

    // By default, all configuration options are true (max. features)
    set(all, Yes);

    // Read the configuration at the current directory
    // TODO(pauek): Later on, look for the file in the parent directories.
    ifstream config_file(CONFIG_FILE);
    if (config_file.is_open()) {
        ConfigReader(config_file).read_config(ID::none);
    }

    // Propagate all values for consistency
    recompute_all();
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
    bool     show_all;

    ConfigDumper(ostream& out_, bool show_all_ = false) : out(out_), show_all(show_all_) {}

    void show_config(ID id) {
        string indent(id.level * 4, ' ');
        auto   state = get(id);
        if (show_all || id.level == 0 || state != No) {
            out << indent << get_name(id) << ": ";
            if (show_all || (id.level == 0 && state == No)) {
                out << state_to_str(state);
            } else {
                out << (state == Yes ? "yes" : "");
            }
            out << endl;
            auto ch = children(id);
            if (show_all || (not ch.empty() && get(id) == Some)) {
                for (auto i = ch.begin; i < ch.end; i++) {
                    show_config(ID(id.level + 1, i));
                }
            }
        }
    }
};

void show(bool all) {
    ConfigDumper(cout, all).show_config(ID(0, 0));
}

// Set an option

void set__(ID id, State new_state) {
    assert(id.level < levels.size());
    assert(id.index < levels[id.level].state.size());

    levels[id.level].state[id.index] = new_state;
}

void recompute_state(ID id) {
    auto ch = children(id);
    if (ch.empty()) {
        return;
    }
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
    set__(id, new_state);
}

void recompute_all() {
    for (int i = levels.size() - 1; i >= 0; i--) {
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
            set__(child_id, new_state);
            propagate_down(child_id, new_state);
        }
    }
}

// Set and propagate
void set(ID id, State new_state) {
    assert(new_state != Some);  // It does not make sense to set a state such as "Some"
    set__(id, new_state);
    propagate_down(id, new_state);
    propagate_up(id);
}

}  // namespace Config
