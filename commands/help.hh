#include <iostream>

#include "command.hh"
using namespace std;

constexpr const char *usage_header =
    "Usage: minicc [options...] [command]\n\n"
    "Simplified C++ compiler and interpreter for educational purposes.\n\n"
    "Commands:\n";

struct Help : Command {
    Help(Args args) : Command(args) {}

    int run() override {
        cout << usage_header;
        size_t longest = Command::max_name_len();
        for (int i = 0; i < ::Command::table.size(); i++) {
            auto   cmd = ::Command::table[i];
            string align_space = string(longest - cmd.name.size() + 4, ' ');
            cout << "   " << cmd.name << align_space << cmd.help << endl;
        }
        cout << endl;
        return 1;
    }

    struct ArgParser : Command::ArgParser {
        ICommand *parse(Args args) override { return new Help(args); }
    };
};
