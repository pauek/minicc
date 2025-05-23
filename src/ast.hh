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

struct Ast;

struct AstNode {
    Ast                      *ast;
    Span                      span;
    std::vector<Error *>      errors;
    std::vector<CommentSeq *> comments;
    AstNode                  *parent;

    void add_error(std::string msg);
    void add_error(Pos ini, Pos fin, std::string msg);

    bool has_errors() const { return !errors.empty(); }

    AstNodeType type() const { return type_; }

   protected:
    AstNodeType type_;
};

template <AstNodeType T>
struct AstDerived : AstNode {
    static bool is_instance(const AstNode *node) { return node->type() == T; }

    AstDerived() { type_ = T; }
};

template <class T>
concept AstNodeClass = std::is_base_of_v<AstNode, T>;

struct Ast {
    template <AstNodeClass T>
    T *create_node() {
        T *n = new T();
        n->ast = this;
        return n;
    }
};

struct Program : public AstDerived<AstNodeType::Program> {
    std::vector<AstNode *> nodes;

    void add(AstNode *n) {
        n->parent = this;
        nodes.push_back(n);
    }
};

struct Include : public AstDerived<AstNodeType::Include> {
    std::string filename;
    bool        global;
};

struct Macro : public AstDerived<AstNodeType::Macro> {
    std::string macro;
};

struct Using : public AstDerived<AstNodeType::Using> {
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
    static Kind TokenToKind(Token::Type toktyp);

    static bool RightAssociative(Kind t) { return t == Expr::Eq; }
    struct Error;
};

template <AstNodeType T>
struct ExprDerived : Expr {
    static bool is_instance(const AstNode *node) { return node->type() == T; }

    ExprDerived() { type_ = T; }
};

struct ExprError : public ExprDerived<AstNodeType::ExprError> {
    std::string code;
};

struct Literal : public ExprDerived<AstNodeType::Literal> {
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

    static std::string Escape(char c, char delim);
    static std::string Escape(std::string s, char delim);
};

struct Identifier : ExprDerived<AstNodeType::Identifier> {
    std::string               name;
    std::vector<Identifier *> prefix;
    std::vector<TypeSpec *>   subtypes;
    bool                      is_namespace = false;  // (used by the interpreter)

    bool is_template() const { return !subtypes.empty(); }

    std::string               TypeStr() const;
    void                      Shift(std::string new_id);
    Identifier               *GetPotentialNamespaceOrClass() const;
    std::vector<Identifier *> GetNonNamespaces();

    static bool is_instance(const AstNode *node) { return node->type() == AstNodeType::Identifier; }
};

struct TypeSpec : public AstDerived<AstNodeType::TypeSpec> {
    enum Qualifier {
        Const = 0b000001,
        Volatile = 0b000010,
        Mutable = 0b000100,
        Register = 0b001000,
        Auto = 0b010000,
        Extern = 0b100000,
    };

    bool        reference = false;
    int8_t      bqual;
    Identifier *id = 0;
    TypeSpec() = default;

    TypeSpec(Identifier *_id) : id(_id), reference(false) {}

    void AddQualifier(Qualifier q) { bqual |= q; }

    bool HasQualifier(Qualifier q) const { return (bqual & q) != 0; }

    bool HasQualifiers() const { return bqual != 0; }

    std::string TypeStr() const;

    bool is_template() const { return !id->subtypes.empty(); }

    Identifier *GetPotentialNamespaceOrClass() const;
};

struct BinaryExpr : public ExprDerived<AstNodeType::BinaryExpr> {
    Kind        kind;
    std::string op, str;
    Expr       *left, *right;
};

struct UnaryExpr : public Expr {
    Expr *expr = 0;
};

template <AstNodeType T>
struct UnaryExprDerived : UnaryExpr {
    UnaryExprDerived() { type_ = T; }

    static bool is_instance(const AstNode *node) { return node->type() == T; }
};

struct SignExpr : public UnaryExprDerived<AstNodeType::SignExpr> {
    enum Kind { Positive, Negative };

    Kind kind;
};

struct IncrExpr : public UnaryExprDerived<AstNodeType::IncrExpr> {
    enum Kind { Positive, Negative };

    Kind kind;
    bool preincr;
};

struct NegExpr : public UnaryExprDerived<AstNodeType::NegExpr> {};

struct AddrExpr : public UnaryExprDerived<AstNodeType::AddrExpr> {};

struct DerefExpr : public UnaryExprDerived<AstNodeType::DerefExpr> {};

struct CallExpr : public ExprDerived<AstNodeType::CallExpr> {
    Expr               *func = 0;
    std::vector<Expr *> args;
};

struct IndexExpr : public ExprDerived<AstNodeType::IndexExpr> {
    Expr *base = 0, *index = 0;
};

struct FieldExpr : public ExprDerived<AstNodeType::FieldExpr> {
    Expr       *base = 0;
    std::string field;
    bool        pointer;
};

struct CondExpr : public ExprDerived<AstNodeType::CondExpr> {
    Expr *cond = 0, *then = 0, *els = 0;
};

struct ExprList : public ExprDerived<AstNodeType::ExprList> {
    std::vector<Expr *> exprs;
};

// Declarations ////////////////////////////////////////////

struct Decl : public AstNode {
    enum Kind { Normal, Pointer };

    TypeSpec   *typespec = 0;
    std::string name;
};

template <AstNodeType T>
struct DeclDerived : Decl {
    static bool is_instance(const AstNode *node) { return node->type() == T; }

    DeclDerived() { type_ = T; }
};

struct VarDecl : public DeclDerived<AstNodeType::VarDecl> {
    Kind kind = Normal;
};

struct ArrayDecl : public DeclDerived<AstNodeType::ArrayDecl> {
    std::vector<Expr *> sizes;
    Kind                kind = Normal;
    std::string         TypeStr() const;
};

struct ObjDecl : public DeclDerived<AstNodeType::ObjDecl> {
    std::vector<Expr *> args;
};

struct Block;

struct FuncDecl : public AstDerived<AstNodeType::FuncDecl> {
    struct Param {
        Pos         ini, fin;
        TypeSpec   *typespec = 0;
        std::string name;
    };

    TypeSpec            *return_typespec;
    Identifier          *id;
    std::vector<Param *> params;
    Block               *block;

    std::string FuncName() const { return id->name; }
};

struct TypedefDecl : public AstDerived<AstNodeType::TypedefDecl> {
    Decl *decl = 0;
};

struct EnumDecl : public AstDerived<AstNodeType::EnumDecl> {
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
struct StmtDerived : Stmt {
    static bool is_instance(const AstNode *node) { return node->type() == T; }

    StmtDerived() { type_ = T; }
};

struct Block : public StmtDerived<AstNodeType::Block> {
    std::vector<Stmt *> stmts;
};

struct StmtError : public StmtDerived<AstNodeType::StmtError> {
    std::string code;
};

struct ExprStmt : public StmtDerived<AstNodeType::ExprStmt> {
    Expr *expr = 0;
    bool  is_return = false;
};

struct IfStmt : public StmtDerived<AstNodeType::IfStmt> {
    Expr *cond = 0;
    Stmt *then = 0, *els = 0;
};

struct ForStmt : public StmtDerived<AstNodeType::ForStmt> {
    Stmt *init = 0;
    Expr *cond = 0, *post = 0;
    Stmt *substmt = 0;
};

struct WhileStmt : public StmtDerived<AstNodeType::WhileStmt> {
    Expr *cond = 0;
    Stmt *substmt = 0;
};

struct DeclStmt : public StmtDerived<AstNodeType::DeclStmt> {
    TypeSpec *typespec;

    struct Item {
        Decl *decl = 0;
        Expr *init = 0;
    };

    std::vector<Item> items;
};

struct StructDecl : public AstDerived<AstNodeType::StructDecl> {
    std::string             name;
    std::vector<DeclStmt *> decls;
    std::string             TypeStr() const;
};

struct JumpStmt : public StmtDerived<AstNodeType::JumpStmt> {
    enum Kind {
        Unknown = -1,
        Break = 0,
        Continue = 1,
        Goto = 2,
    };

    Kind kind = Unknown;

    std::string label;
    static Kind KeywordToType(std::string s);
};

std::string describe(AstNode *node);
bool        has_errors(AstNode *node);
bool        is_read_expr(AstNode *node);
bool        is_write_expr(AstNode *node);
bool        is_assignment(AstNode *node);
void        collect_rights(AstNode *node, std::list<Expr *>& L);

#endif
