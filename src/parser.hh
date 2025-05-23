#ifndef PARSER_H
#define PARSER_H
#include <fstream>
#include <set>
#include "ast.hh"
#include "error.hh"
#include "lexer.hh"

class Parser {
    Lexer                 _lexer;
    std::ostream         *_err;
    std::set<std::string> _types;  // things known as types

    bool _is_type(std::string s) { return _types.find(s) != _types.end(); }

    template <typename X>
    void _skip(X *n) {
        n->comments.push_back(_lexer.skip());
    }

    void _error(AstNode *n, std::string msg, ErrorOptions options = {.stopper = false}) {
        _error(n, n->span, msg, options);
    }

    void _error(AstNode *n, Span span, std::string msg, ErrorOptions options = {.stopper = false}) {
        n->errors.push_back(new Error(span, msg, options));
    }

    void _fatal_error(Pos pos, std::string msg) { throw ParseError(pos, msg); }

    template <class Node>
    typename Node::Error *_error(std::string msg);

    StmtError *_stmt_error(std::string msg);

    void     _parse_expr_seq(AstNode *n, std::vector<Expr *>& v);
    void     _parse_type_seq(AstNode *n, std::vector<TypeSpec *>& v);
    bool     _parse_type_process_token(TypeSpec *type, Token tok, Pos p);
    Decl    *_parse_vardecl(AstNode *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl    *_parse_arraydecl(AstNode *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl    *_parse_objdecl(AstNode *parent, std::string name, CommentSeq *comm);
    ForStmt *_parse_for_classic(ForStmt *stmt);
    ForColonStmt *_parse_for_colon(Stmt *decl);

    std::string _translate_Escapes(std::string s);

   public:
    Parser(std::istream *in, std::ostream *err = &std::cerr);

    const Lexer& lexer() const { return _lexer; }

    AstNode     *parse();
    AstNode     *parse_macro(AstNode *parent);
    AstNode     *parse_using_declaration(AstNode *parent);
    AstNode     *parse_func_or_var(AstNode *parent);
    void         parse_function(FuncDecl *fn);
    Block       *parse_block(AstNode *parent);
    Stmt        *parse_stmt(AstNode *parent);
    Stmt        *parse_iterstmt(AstNode *parent, std::string which);
    Stmt        *parse_while(AstNode *parent);
    Stmt        *parse_for(AstNode *parent);
    Stmt        *parse_ifstmt(AstNode *parent);
    Stmt        *parse_switch(AstNode *parent);
    ExprStmt    *parse_exprstmt(AstNode *parent, bool is_return = false);
    DeclStmt    *parse_declstmt(AstNode *parent, bool is_typedef = false);
    Stmt        *parse_decl_or_expr_stmt(AstNode *parent);
    Stmt        *parse_jumpstmt(AstNode *parent);
    TypeSpec    *parse_typespec(AstNode *parent);
    Identifier  *parse_ident(AstNode *parent, Token tok, Pos ini);
    StructDecl  *parse_struct(AstNode *parent);
    TypedefDecl *parse_typedef(AstNode *parent);
    EnumDecl    *parse_enum(AstNode *parent);
    Expr        *parse_expr(AstNode *parent, Expr::Kind max = Expr::Comma);
    Expr        *parse_primary_expr(AstNode *parent);
    Expr        *parse_postfix_expr(AstNode *parent, Expr *e);
    Expr        *parse_unary_expr(AstNode *parent);
    Expr        *parse_callexpr(Expr *e);
    Expr        *parse_indexexpr(Expr *e);
    Expr        *parse_fieldexpr(Expr *e, Token);
    Expr        *parse_increxpr(Expr *e, Token);
    Expr        *parse_exprlist(AstNode *parent);
};

template <class Node>
typename Node::Error *Parser::_error(std::string msg) {
    typename Node::Error *s = new typename Node::Error();
    s->code = _lexer.skip_to(";");
    _error(s, msg);
    return s;
}

inline AstNode *parse(std::istream& in) {
    return Parser(&in).parse();
}

inline AstNode *parse_file(std::string filename) {
    std::ifstream codefile(filename);
    return parse(codefile);
}
#endif
