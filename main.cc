#include <cassert>
#include <iostream>
using namespace std;

#include "commands/command.hh"
#include "config.hh"
#include "error.hh"

int main(int argc, const char *argv[]) {
    Config::read_from_file();

    Args args(argc, argv);
    try {
        Command::ArgParser arg_parser;

        auto *cmd = arg_parser.parse(args.from(1));
        if (cmd != nullptr) {
            return cmd->run();
        }
    } catch (Error *e) {
        e->print(cerr);
    }
}
