#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>

#include "astcpp.hh"

class Printer {
    std::ostream& out_;

   public:
    Printer(std::ostream& out) : out_(out) {}

    void new_line() { out_ << endl; }

    void print(const IndexExpr& e);
    void print(const Program& P);
    void print(const Macro& macro);
    void print(const Using& usin);
    void print(const BinaryExpr& e);
    void print(const CallExpr& e);
    void print(const FieldExpr& e);
    void print(const Expr& e);
    void print(const Include& include);
};

#endif
