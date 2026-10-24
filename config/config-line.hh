#ifndef CONFIG_LINE_HH
#define CONFIG_LINE_HH

#include "config.hh"

#include <string>

namespace Config {

// Split Config Line

struct Error {
    bool        present;
    std::string message;
    int         lin, col;

    Error() : present(false) {}
    Error(std::string msg, int lin_, int col_)
        : present(true), message(msg), lin(lin_), col(col_) {}
};

struct ConfigLine {
    Error error;

    std::string   name;
    int           level;
    Config::State value;
};

ConfigLine split_config_line(std::string line, int lin);

}  // namespace Config

#endif
