#ifndef AST_H
#define AST_H

#include <algorithm>
#include <cassert>
#include <list>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include "error.hh"

#include "lexer.hh"

template <typename Derived, typename Base>
const bool is_a(const Base *obj) {
    static_assert(std::is_base_of_v<Base, Derived>);
    return Derived::is_instance(obj);
}

template <typename Derived, typename Base>
const Derived *cast(const Base *obj) {
    assert(Derived::is_instance(obj));
    return static_cast<const Derived *>(obj);
};

template <typename Derived, typename Base>
Derived *cast(Base *obj) {
    assert(Derived::is_instance(obj));
    return static_cast<Derived *>(obj);
};

struct Comment {
    enum Kind { None, SingleLine, MultiLine, EndLine };

    Kind        kind;
    std::string text;

    Comment(Kind k = None) : kind(k) {}
};

struct CommentSeq {
    std::vector<Comment> comments;

    bool has_endln() const;
    bool ends_with_empty_line() const;
    void remove_endlns();
    void only_one_endln_at_end();

    bool starts_with_endln() const {
        return !comments.empty() and comments.front().kind == Comment::EndLine;
    }

    bool ends_with_endln() const {
        return !comments.empty() and comments.back().kind == Comment::EndLine;
    }
};

class AstVisitor;
struct TypeSpec;

enum class AstNodeType {
    Program,
    Include,
    Macro,
    Using,
    FuncDecl,
    StructDecl,
    TypedefDecl,
    EnumDecl,
    StmtError,
    ExprStmt,
    IfStmt,
    ForStmt,
    ForColonStmt,
    WhileStmt,
    DeclStmt,
    JumpStmt,
    TypeSpec,
    VarDecl,
    ArrayDecl,
    ObjDecl,
    ExprError,
    Literal,
    Identifier,
    BinaryExpr,
    SignExpr,
    IncrExpr,
    NegExpr,
    AddrExpr,
    DerefExpr,
    CallExpr,
    IndexExpr,
    FieldExpr,
    CondExpr,
    ExprList,
    Block,
};

struct AstNode {
    AstNode                  *parent;
    Span                      span;
    std::vector<Error *>      errors;
    std::vector<CommentSeq *> comments;

    void add_error(std::string msg);
    void add_error(Pos ini, Pos fin, std::string msg);

    bool has_errors() const { return !errors.empty(); }

    AstNodeType type() const { return type_; }

   protected:
    AstNodeType type_;
};

template <AstNodeType T>
struct AstNodeSubtype : AstNode {
    static bool is_instance(const AstNode *node) { return node->type() == T; }

    AstNodeSubtype() { type_ = T; }
};

template <class T>
concept AstNodeClass = std::is_base_of_v<AstNode, T>;

struct Program : public AstNodeSubtype<AstNodeType::Program> {
    std::vector<AstNode *> nodes;

    void add(AstNode *n) {
        n->parent = this;
        nodes.push_back(n);
    }
};

struct Include : public AstNodeSubtype<AstNodeType::Include> {
    std::string filename;
    bool        global;
};

struct Macro : public AstNodeSubtype<AstNodeType::Macro> {
    std::string macro;

    Macro(std::string macro) : macro(macro) {}
};

struct Using : public AstNodeSubtype<AstNodeType::Using> {
    std::string namespc;
};

// Expressions /////////////////////////////////////////////

struct Expr : public AstNode {
    enum Kind {
        Unknown,
        // pm_expression
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
        Comma,
        Infinite
    };

    bool        paren = false;  // if this is true, comments will have an extra element!
    static Kind token_to_kind(Token::Type toktyp);

    static bool right_associative(Kind t) { return t == Expr::Eq; }
    struct Error;
};

template <AstNodeType T>
struct ExprSubtype : Expr {
    static bool is_instance(const AstNode *ast) { return ast->type() == T; }

    ExprSubtype() { type_ = T; }
};

struct ExprError : public ExprSubtype<AstNodeType::ExprError> {
    std::string code;
};

struct Literal : public ExprSubtype<AstNodeType::Literal> {
    enum Kind {
        Bool,
        Int,
        String,
        Char,
        Float,
        Double,
    };

    struct StringData {
        std::string *s;
        bool         L;
    };

    union Data {
        bool       as_bool;
        int        as_int;
        double     as_double;
        char       as_char;
        StringData as_string;
    };

    Kind kind;
    Data val;
    bool L;  // for strings

    static std::string escape(char c, char delim);
    static std::string escape(std::string s, char delim);

    Literal(Kind kind) : kind(kind) {}

    Literal(Kind kind, Data value) : kind(kind), val(value) {}
};

struct Identifier : ExprSubtype<AstNodeType::Identifier> {
    std::string               name;
    std::vector<Identifier *> prefix;
    std::vector<TypeSpec *>   subtypes;
    bool                      is_namespace = false;  // (used by the interpreter)

    bool is_template() const { return !subtypes.empty(); }

    std::string               type_str() const;
    void                      shift(std::string new_id);
    Identifier               *get_potential_namespace_or_class() const;
    std::vector<Identifier *> get_non_namespaces();

    Identifier(std::string _name) : name(_name) {}
};

struct TypeSpec : public AstNodeSubtype<AstNodeType::TypeSpec> {
    enum Qualifier {
        Const = 0b0000001,
        Volatile = 0b0000010,
        Mutable = 0b0000100,
        Register = 0b0001000,
        Extern = 0b0010000,
        Unsigned = 0b0100000,
        Long = 0b1000000,
    };

    bool        reference = false;
    int8_t      bqual = 0;
    Identifier *id = nullptr;
    TypeSpec() = default;

    TypeSpec(Identifier *_id) : id(_id), reference(false) {}

    void add_qualifier(Qualifier q) { bqual |= q; }

    bool has_qualifier(Qualifier q) const { return (bqual & q) != 0; }

    bool has_qualifiers() const { return bqual != 0; }

    std::string type_str() const;

    bool is_template() const { return !id->subtypes.empty(); }

    Identifier *get_potential_namespace_or_class() const;
};

struct BinaryExpr : public ExprSubtype<AstNodeType::BinaryExpr> {
    Kind        kind;
    std::string op, str;
    Expr       *left, *right;

    BinaryExpr(Kind kind = Unknown) : kind(kind) {}
};

struct UnaryExpr : public Expr {
    Expr *expr = nullptr;
};

template <AstNodeType T>
struct UnaryExprSubtype : UnaryExpr {
    UnaryExprSubtype() { type_ = T; }

    static bool is_instance(const AstNode *ast) { return ast->type() == T; }
};

struct SignExpr : public UnaryExprSubtype<AstNodeType::SignExpr> {
    enum Kind { Positive, Negative };

    Kind kind;

    SignExpr(Kind kind) : kind(kind) {}
};

struct IncrExpr : public UnaryExprSubtype<AstNodeType::IncrExpr> {
    enum Kind { Positive, Negative };

    Kind kind;
    bool preincr;

    IncrExpr(Kind kind = Kind::Positive, bool pre = false) : kind(kind), preincr(pre) {}
};

struct NegExpr : public UnaryExprSubtype<AstNodeType::NegExpr> {};

struct AddrExpr : public UnaryExprSubtype<AstNodeType::AddrExpr> {};

struct DerefExpr : public UnaryExprSubtype<AstNodeType::DerefExpr> {};

struct CallExpr : public ExprSubtype<AstNodeType::CallExpr> {
    Expr               *func = nullptr;
    std::vector<Expr *> args;
};

struct IndexExpr : public ExprSubtype<AstNodeType::IndexExpr> {
    Expr *base = nullptr, *index = nullptr;
};

struct FieldExpr : public ExprSubtype<AstNodeType::FieldExpr> {
    Expr       *base = nullptr;
    std::string field;
    bool        pointer;
};

struct CondExpr : public ExprSubtype<AstNodeType::CondExpr> {
    Expr *cond = nullptr, *then = nullptr, *els = nullptr;
};

struct ExprList : public ExprSubtype<AstNodeType::ExprList> {
    std::vector<Expr *> exprs;
};

// Declarations ////////////////////////////////////////////

struct Decl : public AstNode {
    enum Kind { Normal, Pointer };

    TypeSpec   *typespec = nullptr;
    std::string name;
};

template <AstNodeType T>
struct DeclSubtype : Decl {
    static bool is_instance(const AstNode *ast) { return ast->type() == T; }

    DeclSubtype() { type_ = T; }
};

struct VarDecl : public DeclSubtype<AstNodeType::VarDecl> {
    Kind kind = Normal;

    VarDecl(std::string _name, TypeSpec *_typespec = nullptr) {
        typespec = _typespec;
        name = _name;
    }
};

struct ArrayDecl : public DeclSubtype<AstNodeType::ArrayDecl> {
    std::vector<Expr *> sizes;
    Kind                kind = Normal;
    std::string         type_str() const;
};

struct ObjDecl : public DeclSubtype<AstNodeType::ObjDecl> {
    std::vector<Expr *> args;
};

struct Block;

struct FuncDecl : public AstNodeSubtype<AstNodeType::FuncDecl> {
    struct Param {
        Pos         ini, fin;
        TypeSpec   *typespec = nullptr;
        std::string name;

        Param(Pos ini) : ini(ini) {}
    };

    TypeSpec            *return_typespec;
    Identifier          *id;
    std::vector<Param *> params;
    Block               *block;

    std::string func_name() const { return id->name; }

    FuncDecl(Identifier *id) : id(id) {}
};

struct TypedefDecl : public AstNodeSubtype<AstNodeType::TypedefDecl> {
    Decl *decl = nullptr;
};

struct EnumDecl : public AstNodeSubtype<AstNodeType::EnumDecl> {
    struct Value {
        std::string id;
        bool        has_val;
        int         val;

        Value(std::string _id) : id(_id), has_val(false) {}
    };

    std::string        name;
    std::vector<Value> values;
};

// Statements //////////////////////////////////////////////

struct Stmt : public AstNode {};

template <AstNodeType T>
struct StmtSubtype : Stmt {
    static bool is_instance(const AstNode *ast) { return ast->type() == T; }

    StmtSubtype() { type_ = T; }
};

struct Block : public StmtSubtype<AstNodeType::Block> {
    std::vector<Stmt *> stmts;
};

struct StmtError : public StmtSubtype<AstNodeType::StmtError> {
    std::string code;

    StmtError(std::string code) : code(code) {}
};

struct ExprStmt : public StmtSubtype<AstNodeType::ExprStmt> {
    Expr *expr;
    bool  is_return;

    ExprStmt(Expr *e = nullptr, bool ret = false) : expr(e), is_return(ret) {}
};

struct IfStmt : public StmtSubtype<AstNodeType::IfStmt> {
    Expr *cond = nullptr;
    Stmt *then = nullptr, *els = nullptr;
};

struct ForStmt : public StmtSubtype<AstNodeType::ForStmt> {
    Stmt *init = nullptr;
    Expr *cond = nullptr, *post = nullptr;
    Stmt *substmt = nullptr;
};

struct ForColonStmt : public StmtSubtype<AstNodeType::ForColonStmt> {
    Stmt *decl = nullptr;
    Expr *container = nullptr;
    Stmt *substmt = nullptr;
};

struct WhileStmt : public StmtSubtype<AstNodeType::WhileStmt> {
    Expr *cond = nullptr;
    Stmt *substmt = nullptr;
};

struct DeclStmt : public StmtSubtype<AstNodeType::DeclStmt> {
    TypeSpec *typespec;

    struct Item {
        Decl *decl = nullptr;
        Expr *init = nullptr;
    };

    std::vector<Item> items;

    DeclStmt(TypeSpec *t = nullptr) : typespec(t) {}

    void add(Item item) { items.push_back(item); }
};

struct StructDecl : public AstNodeSubtype<AstNodeType::StructDecl> {
    std::string             name;
    std::vector<DeclStmt *> decls;
    std::string             type_str() const;
};

struct JumpStmt : public StmtSubtype<AstNodeType::JumpStmt> {
    enum Kind {
        Unknown = -1,
        Break = 0,
        Continue = 1,
        Goto = 2,
    };

    Kind        kind = Unknown;
    std::string label;
    static Kind keyword_to_type(std::string s);
};

std::string describe(AstNode *ast);
bool        has_errors(AstNode *ast);
bool        is_read_expr(AstNode *ast);
bool        is_write_expr(AstNode *ast);
bool        is_assignment(AstNode *ast);
void        collect_rights(AstNode *ast, std::list<Expr *>& L);

#endif
