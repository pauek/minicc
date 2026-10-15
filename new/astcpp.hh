#ifndef ASTCPP_H
#define ASTCPP_H

#include <string>
#include <vector>

#include "ast.hh"

// <Expressions>
typedef Variant<10, 30> Expr;

// <Literals>
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
// </Literals>

// <UnaryExpressions>
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
// </UnaryExpressions>

struct Identifier : AstType<Identifier, 15> {
    string name;
};
struct BinaryExpr : AstType<BinaryExpr, 20> {
    enum Level { Zero = 0, LogicalOr, LogicalAnd, Equality, Relational, Additive, Multiplicative, Infinite };
    enum Operator { None = 0, Add, Sub, Mul, Div, Mod, And, Eq, NotEq, GT, LT, GE, LE, Or };

    Operator op;
    Expr     left, right;

    static Level op_level(Operator op) {
        switch (op) {
            case Mul:
            case Div:
            case Mod:
                return Multiplicative;
            case Add:
            case Sub:
                return Additive;
            case GT:
            case LT:
            case GE:
            case LE:
                return Relational;
            case Eq:
            case NotEq:
                return Equality;
            case Or:
                return LogicalOr;
            case And:
                return LogicalAnd;
            case None:
                assert(false);
                return Zero;
        }
    }

    static std::string operator_to_str(Operator op) {
        switch (op) {
            case Add:
                return "+";
            case Sub:
                return "-";
            case Mul:
                return "*";
            case Div:
                return "/";
            case Mod:
                return "%";
            case And:
                return "&&";
            case Or:
                return "||";
            case Eq:
                return "==";
            case NotEq:
                return "=";
            case GT:
                return ">";
            case LT:
                return "<";
            case GE:
                return ">=";
            case LE:
                return "<=";
            default:
                assert(false);
        }
    }
};
struct CallExpr : AstType<CallExpr, 21> {
    Expr         func;
    vector<Expr> args;
};
struct FieldExpr : AstType<FieldExpr, 22> {
    Expr   expr;
    string field;
};
struct IndexExpr : AstType<IndexExpr, 23> {
    Expr base, index;
};

// </Expressions>

// <Statements>

typedef Variant<100, 200> Stmt;
struct StmtBlock : AstType<StmtBlock, 100> {
    std::vector<Stmt> stmts = {};
};
struct AssignStmt : AstType<AssignStmt, 102> {
    Ref<Identifier> target;
    Expr            expr;
};
struct ReturnStmt : AstType<ReturnStmt, 101> {
    Expr expr;
};
struct IfStmt : AstType<IfStmt, 103> {
	Expr cond;
	Ref<StmtBlock> if_true, if_false;
};
struct WhileStmt : AstType<WhileStmt, 104> {
	Expr cond;
	Ref<StmtBlock> body;
};
struct ForStmt : AstType<ForStmt, 105> {
	Stmt init, incr;
	Expr cond;
	Ref<StmtBlock> body;
};

// </Statements>

// <TopDeclarations>

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
    Ref<StmtBlock>         body;
};

// </TopDeclarations>

struct Program : AstType<Program, 2000> {
    std::vector<TopDecl> declarations;
};

//

#endif
