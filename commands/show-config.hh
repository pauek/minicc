#include <iostream>
using namespace std;

#include "../config.hh"
#include "command.hh"

struct ShowConfig : Command {
    ShowConfig(Args args) : Command(args) {}

    int run() override {
        Config::read_from_file();
        if (args().empty()) {
            Config::show();
        } else if (num_args() == 1 && (args()[0] == "--show-all" || args()[0] == "-a")) {
            Config::show_all_options();
        } else {
            cerr << "Unknown args:";
            for (auto arg : args()) {
                cerr << " '" << arg << "'";
            }
            cerr << endl;
            return 1;
        }

        return 0;
    }

    struct ArgParser : Command::ArgParser {
        ICommand *parse(Args args) override { return new ShowConfig(args); }
    };
};
