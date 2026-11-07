#include <iostream>
using namespace std;

#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

struct Lex : Command {
    Lex(Args args) : Command(args) {}

    int run() override {
        for (auto filename : args()) {
            string text = read_file(filename);
            if (num_args() > 1) {
                cout << filename << ":" << endl;
            }
            try {
                Lexer lexer(text, filename);
                lexer.run();
                lexer.tokens().dump(cout);
            } catch (Error *e) {
                e->print(cerr);
                return 1;
            }
        }
        return 0;
    }

    static void show_help() {
        constexpr const char *lex_usage =
            "Usage: minicc lex [options...] [file]...\n"
            "\n"
            "Lists C++ tokens in file(s).\n"
            "\n"
            "Options:\n"
            "    none for now... ;)\n"
            "\n";

        cout << lex_usage;
    }

    struct ArgParser : Command::ArgParser {
        ICommand *parse(Args args) override {
            if (args.empty()) {
                Lex::show_help();
                return nullptr;
            }
            return new Lex(args);
        }
    };
};
