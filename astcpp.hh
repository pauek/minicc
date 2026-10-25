#ifndef ASTCPP_H
#define ASTCPP_H

#include <string>
#include <vector>

#include "datatype.hh"

// Type Specifier
struct Identifier;
struct TypeSpec : DataType<TypeSpec, 1003> {
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

    static Qualifier   qualifier_from_str(std::string name);
    static std::string qualifier_to_str(Qualifier qual);
};

// <Expressions>
typedef Variant<10, 30> Expr;

// <Literals>
typedef Variant<10, 14> Literal;

struct BoolLiteral : DataType<BoolLiteral, 10> {
    bool value;
};
struct IntLiteral : DataType<IntLiteral, 11> {
    int value;
};
struct DoubleLiteral : DataType<DoubleLiteral, 12> {
    double value;
};
struct CharLiteral : DataType<CharLiteral, 13> {
    char value;
};
struct StringLiteral : DataType<StringLiteral, 14> {
    string value;
};
// </Literals>

// <UnaryExpressions>
typedef Variant<16, 18> UnaryExpr;

struct SignExpr : DataType<SignExpr, 16> {
    Expr expr;
    bool negative;
};
struct NotExpr : DataType<NotExpr, 18> {
    Expr expr;
};
// </UnaryExpressions>

struct Identifier : DataType<Identifier, 15> {
    string name;
};
struct BinaryExpr : DataType<BinaryExpr, 20> {
    enum Level {
        Zero = 0,
        LogicalOr,
        LogicalAnd,
        Equality,
        Relational,
        Additive,
        Multiplicative,
        Infinite
    };
    enum Operator { None = 0, Add, Sub, Mul, Div, Mod, And, Eq, NotEq, GT, LT, GE, LE, Or };

    Operator op;
    Expr     left, right;

    static Level op_level(Operator op);

    static std::string operator_to_str(Operator op);
};
struct CallExpr : DataType<CallExpr, 21> {
    Expr         func;
    vector<Expr> args;
};
struct FieldExpr : DataType<FieldExpr, 22> {
    Expr   expr;
    string field;
};
struct IndexExpr : DataType<IndexExpr, 23> {
    Expr base, index;
};

typedef Variant<24, 25> ForIncrExpr;

struct AssignExpr : DataType<AssignExpr, 24> {
    enum Operator { Normal, Add, Sub, Mul, Div, Mod, Error };

    Ref<Identifier> target;
    Expr            expr;
    Operator        op;

    static bool     is_assign_op(TypeID id);
    static Operator token_to_op(TypeID id);
    static std::string op_to_str(Operator op);
};

struct IncrExpr : DataType<IncrExpr, 25> {
    enum Operator { Add, Sub, Error };
    enum Order { Pre, Post };

    static bool is_incr_op(TypeID id);
    static Operator token_to_op(TypeID id);
    static string op_to_str(Operator op);

    Ref<Identifier> target;
    Operator        op;
    Order           order;
};

// This is a piece of an if statement
struct StmtBlock;
struct CondExpr : DataType<CondExpr, 26> {
    Expr           cond;
    Ref<StmtBlock> then;
};
// </Expressions>

// <Statements>

typedef Variant<100, 200> Stmt;
struct StmtBlock : DataType<StmtBlock, 100> {
    std::vector<Stmt> stmts = {};
};
struct AssignStmt : DataType<AssignStmt, 102> {
    Ref<AssignExpr> assign;
};
struct IncrStmt : DataType<IncrStmt, 103> {
    Ref<IncrExpr> incr;
};
struct ReturnStmt : DataType<ReturnStmt, 104> {
    Expr expr;
};
struct IfStmt : DataType<IfStmt, 105> {
    vector<Ref<CondExpr>> cond_exprs;
    Ref<StmtBlock>        els;
};
struct WhileStmt : DataType<WhileStmt, 106> {
    Expr           cond;
    Ref<StmtBlock> body;
};

typedef Variant<120, 121> ForInitStmt;
struct ForInitAssign : DataType<ForInitAssign, 120> {
    Ref<AssignExpr> assign;
};
struct ForInitDecl : DataType<ForInitDecl, 121> {
    Ref<TypeSpec>              type;
    vector<Ref<ForInitAssign>> variables;
};
struct ForStmt : DataType<ForStmt, 122> {
    ForInitStmt    init;
    Expr           cond;
    ForIncrExpr    incr;
    Ref<StmtBlock> body;
};

// </Statements>

// <TopDeclarations>

typedef Variant<1000, 1100> TopDecl;

// Include
struct Include : DataType<Include, 1000> {
    string filename;
    bool   global;
};

// PreprocessorMacro
struct Macro : DataType<Macro, 1001> {
    string name;
    size_t start, end;  // starting and ending tokens
};

// Using declaration
struct Using : DataType<Using, 1002> {
    string namespc;
};

struct FuncParam : DataType<FuncParam, 1004> {
    Ref<TypeSpec>   type;
    Ref<Identifier> ident;
};

struct FuncDecl : DataType<FuncDecl, 1005> {
    Ref<TypeSpec>          return_type;
    Ref<Identifier>        ident;
    vector<Ref<FuncParam>> params;
    Ref<StmtBlock>         body;
};

// </TopDeclarations>

struct Program : DataType<Program, 2000> {
    std::vector<TopDecl> declarations;
};

//

#endif
