#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>

#include "astcpp.hh"
#include "tokens.hh"

class AstDumper {
    std::ostream& out_;
    const Tokens& tokens_;

   public:
    AstDumper(std::ostream& out, const Tokens& tokens) : out_(out), tokens_(tokens) {}

    void new_line() { out_ << endl; }

    void dump(const Program& P);


    void dump(const Macro& macro);
    void dump(const Using& usin);
    void dump(const Include& include);
    void dump(const FuncDecl& funcdecl);

    void dump(const TypeSpec& typespec);
    void dump(const Identifier& ident);
    void dump(const FuncParam& funcparam);

};

#endif
