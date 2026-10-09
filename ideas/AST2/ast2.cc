#include <cassert>
#include <iostream>
using namespace std;

#include "astcpp.hh"
#include "printer.hh"

int main() {
    auto ie = IndexExpr::make({
        .base = Expr(Identifier::make({.name = "a"})),
        .index = Expr(IntLiteral::make({.value = 1})),
    });

    auto e = Include::make({.filename = "iostream"});

    auto pr = Printer(cout);
    pr.print(ie.get());
    pr.print(e.get());
}
