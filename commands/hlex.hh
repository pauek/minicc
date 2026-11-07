#include <iostream>
using namespace std;

#include "../hl-lexer.hh"
#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

struct HLex : Command {
    HLex(Args args) : Command(args) {}

    void hlex_file(string filename, bool show_filename) {
        string text = read_file(filename);
        if (show_filename) {
            cout << filename << ":" << endl;
        }

        Lexer lexer(text, filename);
        lexer.run();

        HLexer hl_lex(lexer);
        hl_lex.run();
        hl_lex.dump(cout);
    }

    int run() override {
        for (auto filename : args()) {
            try {
                hlex_file(filename, num_args() > 1);
            } catch (Error *e) {
                e->print(cerr);
            }
        }
        return 0;
    }

    struct ArgParser : Command::ArgParser {
    	ICommand *parse(Args args) override { return new HLex(args); }
    };
};
