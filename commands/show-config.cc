#include <iostream>
using namespace std;

#include "../config.hh"
#include "command.hh"

int ShowConfig(Args args) {
    Config::read_from_file();

    if (args.empty()) {
        Config::show();
    } else if (args.size() == 1 && (args[0] == "--show-all" || args[0] == "-a")) {
        Config::show_all_options();
    } else {
        cerr << "Unknown args:";
        for (auto arg : args) {
            cerr << " '" << arg << "'";
        }
        cerr << endl;
        return 1;
    }

    return 0;
}
