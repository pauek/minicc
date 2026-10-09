#ifndef ASTCPP_H
#define ASTCPP_H

#include <string>

#include "ast2.hh"

// Expressions 3-15
typedef Variant<10, 30> Expr;

// Literals
typedef Variant<10, 14> Literal;

struct BoolLiteral : AstType<BoolLiteral, 10> {
    bool value;
};

struct IntLiteral : AstType<IntLiteral, 11> {
    int value;
};

struct DoubleLiteral : AstType<DoubleLiteral, 12> {
    double value;
};

struct CharLiteral : AstType<CharLiteral, 13> {
    char value;
};

struct StringLiteral : AstType<StringLiteral, 14> {
    string value;
};

// Identifier

struct Identifier : AstType<Identifier, 15> {
    string name;
};

// Unary Expressions

typedef Variant<16, 18> UnaryExpr;

struct UnaryExpr_ {
    Expr expr;
};

struct SignExpr : UnaryExpr_, AstType<SignExpr, 16> {
    bool negative;
};

struct IncrExpr : UnaryExpr_, AstType<SignExpr, 17> {
    bool negative;
    bool pre;
};

struct NotExpr : UnaryExpr_, AstType<NotExpr, 18> {};

// Binary Expression

struct BinaryExpr : AstType<BinaryExpr, 20> {
    enum Kind {
        Multiplicative,
        Additive,
        Shift,
        Relational,
        Equality,
        BitAnd,
        BitXor,
        BitOr,
        LogicalAnd,
        LogicalOr,
        Conditional,
        Eq,
        Comma
    };

    Kind   kind;
    string op;
    Expr   left, right;
};

struct IndexExpr : AstType<IndexExpr, 21> {
    Expr base, index;
};

//

struct Include : AstType<Include, 100> {
    string filename;
    bool   global;
};

struct Macro : AstType<Macro, 101> {
    string macro;
};

struct Using : AstType<Using, 102> {
    string namespc;
};

struct TypeSpecifier : AstType<TypeSpecifier, 103> {
    enum Qualifier {
        Const = 0b000001,
        Volatile = 0b000010,
        Mutable = 0b000100,
        Register = 0b001000,
        Auto = 0b010000,
        Extern = 0b100000,
    };

    bool            reference = false;
    int8_t          bqual;
    Ref<Identifier> ident;
};

#endif
