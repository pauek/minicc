#include <cassert>
#include <iostream>
using namespace std;

#include "astcpp.hh"
#include "lexer.hh"
#include "printer.hh"
#include "utils.hh"

void demo_print() {
    auto pr = Printer(cout);

    auto ie = IndexExpr::make({
        .base = Expr(Identifier::make({.name = "a"})),
        .index = Expr(IntLiteral::make({.value = 1})),
    });
    pr.print(ie.get());
    pr.new_line();

    auto e = Include::make({.filename = "iostream"});
    pr.print(e.get());

    auto be = BinaryExpr::make({
        .level = BinaryExpr::Level::Relational,
        .op = BinaryExpr::Operator::Eq,
        .left = Expr(Identifier::make({.name = "b"})),
        .right = Expr(IntLiteral::make({.value = 3})),
    });
    pr.print(be.get());
    pr.new_line();

    auto fe = FieldExpr::make({
        .expr = Expr(Identifier::make({.name = "tuple"})),
        .field = "x",
    });
    pr.print(fe.get());
    pr.new_line();
}

int main(int argc, char *argv[]) {
    if (argc >= 3) {
        cerr << "Usage: hldb [<file>]" << endl;
        exit(1);
    }

    string filename(argc == 1 ? "" : argv[1]);
    bool   from_stdin = filename == "-" or filename.empty();

    string input = (from_stdin ? read_stdin() : read_file(filename));

    try {
        Lexer lexer(input, filename);
        auto  tokens = lexer.run();
        tokens.dump(cout);

    } catch (LexerError *e) {
        cerr << filename << ":" << e->pos << ": Lexer error: " << e->msg << endl;
    }
}
