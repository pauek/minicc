#ifndef ASTCPP_H
#define ASTCPP_H

#include <string>
#include <vector>

#include "ast.hh"

// Expressions
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

struct SignExpr : AstType<SignExpr, 16> {
    Expr expr;
    bool negative;
};

struct IncrExpr : AstType<IncrExpr, 17> {
    Expr expr;
    bool negative;
    bool pre;
};

struct NotExpr : AstType<NotExpr, 18> {
    Expr expr;
};

// Binary Expression
struct BinaryExpr : AstType<BinaryExpr, 20> {
    enum Level {
        Multiplicative,
        Additive,
        Relational,
        Equality,
        LogicalAnd,
        LogicalOr,
    };

    enum Operator {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        And,
        Eq,
        NotEq,
        GT,
        LT,
        GE,
        LE,
        Or,
    };

    Level    level;
    Operator op;
    Expr     left, right;
};

struct CallExpr : AstType<CallExpr, 21> {
    Expr         func;
    vector<Expr> args;
};

struct FieldExpr : AstType<FieldExpr, 22> {
    Expr   expr;
    string field;
};

// End Expr

// IndexExpr
struct IndexExpr : AstType<IndexExpr, 21> {
    Expr base, index;
};

typedef Variant<1000, 1100> TopDecl;

// Include
struct Include : AstType<Include, 1000> {
    string filename;
    bool   global;
};

// PreprocessorMacro
struct Macro : AstType<Macro, 1001> {
    string name;
    size_t start, end;  // starting and ending tokens
};

// Using declaration
struct Using : AstType<Using, 1002> {
    string namespc;
};

// Type Specifier
struct TypeSpec : AstType<TypeSpec, 1003> {
    enum Qualifier {
        None = 0b000000,
        Const = 0b000001,
        Volatile = 0b000010,
        Mutable = 0b000100,
        Register = 0b001000,
        Auto = 0b010000,
        Extern = 0b100000,
    };

    Ref<Identifier> ident;

    bool   reference = false;
    int8_t bqual = None;

    static Qualifier qualifier_from_str(std::string name) {
        if (name == "auto") {
            return Auto;
        } else if (name == "const") {
            return Const;
        } else {
            assert(false);
        }
    }

    static std::string qualifier_to_str(Qualifier qual) {
        switch (qual) {
            case Auto:
                return "auto";
            case Const:
                return "const";
            default:
                assert(false);
        }
    }
};

struct FuncParam : AstType<FuncParam, 1004> {
    Ref<TypeSpec>   type;
    Ref<Identifier> ident;
};

struct FuncDecl : AstType<FuncDecl, 1005> {
    Ref<TypeSpec>          return_type;
    Ref<Identifier>        ident;
    vector<Ref<FuncParam>> params;
};

struct Program : AstType<Program, 2000> {
    std::vector<TopDecl> declarations;
};

//

#endif
