#include <format>
#include <fstream>
#include <iostream>
using namespace std;

#include "config.hh"

Config Config::global_config_;

template <class... Args>
void error(std::format_string<Args...> fmt, Args&&...args) {
    cerr << std::format(fmt, args...) << endl;
    exit(1);
}

bool is_config_name(char c) {
    return isalpha(c) || isdigit(c) || c == '_';
}

bool allowed_char(char c) {
    return isspace(c) || is_config_name(c) || c == '.';
}

bool all_spaces(std::string line) {
    for (char c : line) {
        if (not isspace(c)) {
            return false;
        }
    }
    return true;
}

class ConfigLine {
    const string& line_;
    size_t        lin_, col_;
    int           indent_;

    bool finished() const { return col_ - 1 >= line_.size(); }
    char curr() const { return line_[col_ - 1]; }
    bool at(char c) const { return curr() == c; }

    void check_word() {
        while (finished() && not at('.')) {
            if (not is_config_name(curr())) {
                error(".minicc:{}:{}: Unexpected char '{}'.", lin_, col_, line_[col_ - 1]);
            }
            col_++;
        }
    }

    void check_indent() {
        while (isspace(curr())) {
            col_++;
        }
        indent_ = col_ - 1;
    }

   public:
    ConfigLine(const string& line, size_t lin) : line_(line), lin_(lin), col_(1) {}

    const int& indent() const { return indent_; }
    string     name() const { return line_.substr(indent_); }

    void check() {
        check_indent();
        while (true) {
            check_word();
            if (finished()) {
                return;
            }
            if (not at('.')) {
                error(".minicc:{}:{}: Expected '.'.", lin_, col_);
            }
            col_++;
        }
    }
};

void Config::read_from_file_(std::string filename) {
    ifstream       fin(filename);
    string         line;
    int            current_indent = -1, lin = 1;
    vector<string> path = {};
    while (getline(fin, line)) {
        if (all_spaces(line)) {
            continue;
        }
        ConfigLine config_line(line, lin);
        config_line.check();

        // Update current indent and prefix
        if (current_indent == -1) {
            // The first, time just set the indent
            if (config_line.indent() != 0) {
                error(".minicc:{}:{}: config: wrong first indentation!", lin, config_line.indent());
            }
            current_indent = config_line.indent();
            path = {config_line.name()};
        } else if (config_line.indent() > current_indent) {
            if (config_line.indent() != current_indent + 1) {
                error(".minicc:{}:{}: Indent difference > 1!", line, config_line.indent());
            }
            path.push_back(config_line.name());
        } else if (config_line.indent() < current_indent) {
            path.pop_back();
        }

        // Process the thing...
    }
}
