#include <fstream>
using namespace std;

#include "config.hh"

// utils //

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

// ConfigLines //

ConfigLines::Line ConfigLines::parse_line(string line) {
    col_ = 0;

    // indentation
    while (isspace(line[col_])) {
        col_++;
    }
    if (col_ % 4 != 0) {
        error(".minicc:{}:{}: Indentation is 4 spaces exactly.", lin_, col_);
    }
    size_t indent = col_;
    size_t level = indent / 4;

    // Name
    while (col_ < line.size() && is_config_name(line[col_])) {
        col_++;
    }
    if (col_ >= line.size() || line[col_] != ':') {
        error(".minicc:{}:{}: Expected ':'.", lin_, col_);
    }
    string name = line.substr(indent, col_ - indent);

    // Value
    col_++;
    while (col_ < line.size() && isspace(line[col_])) {
        col_++;
    }
    State state = No;
    if (line.substr(col_, 3) == "yes") {
        state = Yes;
        col_ += 3;
    } else if (line.substr(col_, 2) == "no") {
        state = No;
        col_ += 2;
    } else {
        error(".minicc:{}:{}: Expected 'yes' or 'no'.", lin_, col_);
    }

    return Line{.level = level, .name = name, .state = state};
}

void ConfigLines::next() {
    lin_++;
    string line;
    getline(file_, line);
    while (all_spaces(line)) {
        getline(file_, line);
    }

    auto config_line = parse_line(line);
    int  indent = config_line.level * 4;

    // Update current indent and prefix
    if (config_line.level == size_t(-1)) {
        // The first, time just set the indent
        if (config_line.level != 0) {
            error(".minicc:{}:{}: config: wrong first indentation!", lin_, indent);
        }
    } else if (indent > config_line.level) {
        if (indent != config_line.level + 1) {
            error(".minicc:{}:{}: Indent difference > 1!", line, indent);
        }
    }
    config_line_ = config_line;
}

ConfigLines::ConfigLines(std::string filename) : file_(filename) {
    config_line_ = {size_t(-1), "", No};
    next();
}
