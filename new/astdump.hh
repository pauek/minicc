#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>

#include "astcpp.hh"
#include "tokens.hh"

class AstDumper {
    std::ostream& out_;
    const Tokens& tokens_;
    int           indent_ = 0;

    string indent() const { return string(4 * indent_, ' '); }
    void   indent(int di) { indent_ += di; }

   public:
    AstDumper(std::ostream& out, const Tokens& tokens) : out_(out), tokens_(tokens) {}

    void new_line() { out_ << endl; }

    void dump(const Program& P);

    void dump(Expr expr);
    void dump(const BinaryExpr& expr);
    void dump(const AssignExpr& expr);

    void dump(Stmt S);
    void dump(const StmtBlock& block);
    void dump(const ReturnStmt& stmt);
    void dump(const AssignStmt& stmt);
    void dump(const IfStmt& stmt);
    void dump(const WhileStmt& stmt);

    void dump(const ForInitStmt& stmt);
    void dump(const ForInitAssign& assing);
    void dump(const ForInitDecl& decl);
    void dump(const ForStmt& stmt);

    void dump(const Macro& macro);
    void dump(const Using& usin);
    void dump(const Include& include);
    void dump(const FuncDecl& funcdecl);

    void dump(const TypeSpec& typespec);
    void dump(const Identifier& ident);
    void dump(const FuncParam& funcparam);
};

#endif
