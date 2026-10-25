#ifndef COMMANDS_HH
#define COMMANDS_HH

#include <cstring>
#include <iostream>
#include <vector>

struct Args {
    int    nargs;
    char **args;

    Args(int _nargs, char **_args) : nargs(_nargs), args(_args) {
        shift();  // Discard program name
    }

    std::string shift() {
        std::string result = args[0];
        args++, nargs--;
        return result;
    }

    bool empty() { return nargs == 0; }
};

typedef int (*CmdFunc)(Args& args);

struct Command {
    const char *name;
    CmdFunc     func;
    const char *help;
};

inline const Command *find_command(std::string name, const std::vector<Command>& commands) {
    for (const auto& cmd : commands) {
        if (cmd.name == name) {
            return &cmd;
        }
    }
    return nullptr;
}

inline void help(const std::vector<Command>& commands) {
    std::cout << "usage: minicc <command> [...args]" << std::endl << std::endl;
    std::cout << "Commands: " << std::endl;
    std::cout << "  help" << std::endl;
    size_t max_width = 0;
    for (const auto& cmd : commands) {
        max_width = std::max(max_width, std::strlen(cmd.name));
    }
    for (const auto& cmd : commands) {
        std::string __space__(max_width - std::strlen(cmd.name) + 4, ' ');
        std::cout << "  " << cmd.name << __space__ << cmd.help << std::endl;
    }
    std::cout << std::endl;
    exit(1);
}

#endif