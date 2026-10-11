#include <cassert>
#include <iostream>
using namespace std;

#include "command.hh"
#include "lexer.hh"

ICommand *parse_args(int argc, char *argv[]) {
	if (argc < 2) {
		return new Command<Help>({});
	}

    // Determine command name and extract arguments/options
    string         cmd_name = argv[1];
    vector<string> args(argv + 2, argv + argc);

    // Dispatch
    if (cmd_name == "lex") {
        return new Command<Lex>(args);
    }

    cerr << "Unknown command '" << cmd_name << "'." << endl;
    return new Command<Help>(args);
}

int main(int argc, char *argv[]) {
    try {
        auto *command = parse_args(argc, argv);
        return command->run();
    } catch (LexerError *e) {
        e->print(cerr);
    }
}
