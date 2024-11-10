#ifndef PARSER_H
#define PARSER_H
#include <set>
#include "ast.hh"
#include "lexer.hh"

struct ParseError {
    Pos         pos;
    std::string msg;

    ParseError() : pos(-1, 0) {}

    ParseError(Pos p, std::string m) : pos(p), msg(m) {}

    bool empty() const { return pos.lin == -1; }
};

class Parser {
    Lexer            _lexer;
    std::ostream    *_err;
    set<std::string> _types;  // things known as types
    bool             _is_type(std::string);

    template <typename X>
    void _skip(X *n) {
        n->comments.push_back(_lexer.skip());
    }

    void error(Ast *n, std::string msg);
    void error(Ast *n, Span span, std::string msg);
    void stopper_error(Ast *n, std::string msg);
    void stopper_error(Ast *n, Span span, std::string msg);
    void fatal_error(Pos p, std::string msg);

    template <class Node>
    typename Node::Error *error(std::string msg);

    StmtError *stmt_error(std::string msg);
    void       parse_expr_seq(Ast *n, std::vector<Expr *>& v);
    void       parse_type_seq(Ast *n, std::vector<TypeSpec *>& v);
    bool       _parse_type_process_token(TypeSpec *type, Token tok, Pos p);
    Decl      *_parse_vardecl(Ast *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl      *_parse_arraydecl(Ast *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl      *_parse_objdecl(Ast *parent, std::string name, CommentSeq *comm);

    std::string _translate_Escapes(std::string s);

   public:
    Parser(std::istream *in, std::ostream *err = &std::cerr);

    const Lexer& lexer() const { return _lexer; }

    Ast         *parse();
    Ast         *parse_macro(Ast *parent);
    Ast         *parse_using_declaration(Ast *parent);
    Ast         *parse_func_or_var(Ast *parent);
    void         parse_function(FuncDecl *fn);
    Block       *parse_block(Ast *parent);
    Stmt        *parse_stmt(Ast *parent);
    Stmt        *parse_iterstmt(Ast *parent, string which);
    Stmt        *parse_while(Ast *parent);
    Stmt        *parse_for(Ast *parent);
    Stmt        *parse_ifstmt(Ast *parent);
    Stmt        *parse_switch(Ast *parent);
    ExprStmt    *parse_exprstmt(Ast *parent, bool is_return = false);
    DeclStmt    *parse_declstmt(Ast *parent, bool is_typedef = false);
    Stmt        *parse_decl_or_expr_stmt(Ast *parent);
    Stmt        *parse_jumpstmt(Ast *parent);
    TypeSpec    *parse_typespec(Ast *parent);
    Identifier  *parse_ident(Ast *parent, Token tok, Pos ini);
    StructDecl  *parse_struct(Ast *parent);
    TypedefDecl *parse_typedef(Ast *parent);
    EnumDecl    *parse_enum(Ast *parent);
    Expr        *parse_expr(Ast *parent, Expr::Kind max = Expr::Comma);
    Expr        *parse_primary_expr(Ast *parent);
    Expr        *parse_postfix_expr(Ast *parent, Expr *e);
    Expr        *parse_unary_expr(Ast *parent);
    Expr        *parse_callexpr(Expr *e);
    Expr        *parse_indexexpr(Expr *e);
    Expr        *parse_fieldexpr(Expr *e, Token);
    Expr        *parse_increxpr(Expr *e, Token);
    Expr        *parse_exprlist(Ast *parent);
};

inline Ast *parse(std::istream& in) {
    return Parser(&in).parse();
}

inline Ast *parse_file(string filename) {
    ifstream codefile(filename);
    return parse(codefile);
}
#endif
