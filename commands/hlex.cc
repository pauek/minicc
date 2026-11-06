#include <iostream>
using namespace std;

#include "../hl-lexer.hh"
#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

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

int HLex(Args args) {
    for (auto filename : args) {
        try {
        	hlex_file(filename, args.size() > 1);
        } catch (Error *e) {
            e->print(cerr);
        }
    }
    return 0;
}
