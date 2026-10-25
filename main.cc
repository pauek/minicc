#include <cassert>
#include <iostream>
using namespace std;

#include "command.hh"
#include "config.hh"
#include "error.hh"

ICommand *parse_args(Args args) {
    if (args.empty()) {
        return new Command<Help>(args);
    }

    // Dispatch on command
    const string command_name = args[0];
    if (command_name == "lex") {
        return parse_args<Lex>(args.from(1));
    } else if (command_name == "hlex") {
        return parse_args<HLex>(args.from(1));
    } else if (command_name == "parse") {
        return parse_args<Parse>(args.from(1));
    } else if (command_name == "config") {
        return parse_args<ShowConfig>(args.from(1));
    }

    cerr << "Unknown command '" << command_name << "'." << endl;
    return new Command<Help>(args);
}

int main(int argc, char *argv[]) {
    Config::read_from_file();

    Args args(argc, argv);
    try {
        auto *command = parse_args(args.from(1));
        return command->run();
    } catch (Error *e) {
        e->print(cerr);
    }
}
