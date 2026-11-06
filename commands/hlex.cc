#include <iostream>
using namespace std;

#include "../hl-lexer.hh"
#include "../lexer.hh"
#include "../utils.hh"
#include "command.hh"

int HLex(Args args) {
    for (auto filename : args) {
        string text = read_file(filename);
        if (args.size() > 1) {
            cout << filename << ":" << endl;
        }
        try {
            Lexer lexer(text, filename);
            lexer.run();
            HLexer hl_lex(lexer);
            hl_lex.run();
            hl_lex.dump(cout);
        } catch (Error *e) {
            e->print(cerr);
        }
    }
    return 0;
}
