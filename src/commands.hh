#ifndef COMMANDS_HH
#define COMMANDS_HH

#include <cstring>
#include <string>

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

inline const Command *find_command(string name, const vector<Command>& commands) {
    for (const auto& cmd : commands) {
        if (cmd.name == name) {
            return &cmd;
        }
    }
    return nullptr;
}

inline void help(const vector<Command>& commands) {
    cout << "usage: minicc <command> [...args]" << endl << endl;
    cout << "Commands: " << endl;
    cout << "  help" << endl;
    size_t max_width = 0;
    for (const auto& cmd : commands) {
        max_width = max(max_width, std::strlen(cmd.name));
    }
    for (const auto& cmd : commands) {
        string __space__(max_width - std::strlen(cmd.name) + 4, ' ');
        cout << "  " << cmd.name << __space__ << cmd.help << endl;
    }
    cout << endl;
    exit(1);
}

#endif