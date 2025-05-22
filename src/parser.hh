#ifndef PARSER_H
#define PARSER_H
#include <fstream>
#include <set>
#include "ast.hh"
#include "error.hh"
#include "lexer.hh"

class Parser {
    Ast                  *_ast;
    Lexer                 _lexer;
    std::ostream         *_err;
    std::set<std::string> _types;  // things known as types

    bool _is_type(std::string s) { return _types.find(s) != _types.end(); }

    template <typename X>
    void _skip(X *n) {
        n->comments.push_back(_lexer.skip());
    }

    void _error(AstNodeCore *n, std::string msg, ErrorOptions options = {.stopper = false}) {
        _error(n, n->span, msg, options);
    }

    void _error(AstNodeCore *n, Span span, std::string msg, ErrorOptions options = {.stopper = false}) {
        n->errors.push_back(new Error(span, msg, options));
    }

    void _fatal_error(Pos pos, std::string msg) { throw ParseError(pos, msg); }

    template <class Node>
    typename Node::Error *_error(std::string msg);

    StmtError *_stmt_error(std::string msg);

    void  _parse_expr_seq(AstNodeCore *n, std::vector<Expr *>& v);
    void  _parse_type_seq(AstNodeCore *n, std::vector<TypeSpec *>& v);
    bool  _parse_type_process_token(TypeSpec *type, Token tok, Pos p);
    Decl *_parse_vardecl(AstNodeCore *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl *_parse_arraydecl(AstNodeCore *parent, std::string name, Decl::Kind kind, CommentSeq *comm);
    Decl *_parse_objdecl(AstNodeCore *parent, std::string name, CommentSeq *comm);

    std::string _translate_Escapes(std::string s);

   public:
    Parser(std::istream *in, std::ostream *err = &std::cerr);

    const Lexer& lexer() const { return _lexer; }

    AstNodeCore     *parse();
    AstNodeCore     *parse_macro(AstNodeCore *parent);
    AstNodeCore     *parse_using_declaration(AstNodeCore *parent);
    AstNodeCore     *parse_func_or_var(AstNodeCore *parent);
    void         parse_function(FuncDecl *fn);
    Block       *parse_block(AstNodeCore *parent);
    Stmt        *parse_stmt(AstNodeCore *parent);
    Stmt        *parse_iterstmt(AstNodeCore *parent, std::string which);
    Stmt        *parse_while(AstNodeCore *parent);
    Stmt        *parse_for(AstNodeCore *parent);
    Stmt        *parse_ifstmt(AstNodeCore *parent);
    Stmt        *parse_switch(AstNodeCore *parent);
    ExprStmt    *parse_exprstmt(AstNodeCore *parent, bool is_return = false);
    DeclStmt    *parse_declstmt(AstNodeCore *parent, bool is_typedef = false);
    Stmt        *parse_decl_or_expr_stmt(AstNodeCore *parent);
    Stmt        *parse_jumpstmt(AstNodeCore *parent);
    TypeSpec    *parse_typespec(AstNodeCore *parent);
    Identifier  *parse_ident(AstNodeCore *parent, Token tok, Pos ini);
    StructDecl  *parse_struct(AstNodeCore *parent);
    TypedefDecl *parse_typedef(AstNodeCore *parent);
    EnumDecl    *parse_enum(AstNodeCore *parent);
    Expr        *parse_expr(AstNodeCore *parent, Expr::Kind max = Expr::Comma);
    Expr        *parse_primary_expr(AstNodeCore *parent);
    Expr        *parse_postfix_expr(AstNodeCore *parent, Expr *e);
    Expr        *parse_unary_expr(AstNodeCore *parent);
    Expr        *parse_callexpr(Expr *e);
    Expr        *parse_indexexpr(Expr *e);
    Expr        *parse_fieldexpr(Expr *e, Token);
    Expr        *parse_increxpr(Expr *e, Token);
    Expr        *parse_exprlist(AstNodeCore *parent);
};

template <class Node>
typename Node::Error *Parser::_error(std::string msg) {
    typename Node::Error *s = new typename Node::Error();
    s->code = _lexer.skip_to(";");
    _error(s, msg);
    return s;
}

inline AstNodeCore *parse(std::istream& in) {
    return Parser(&in).parse();
}

inline AstNodeCore *parse_file(std::string filename) {
    std::ifstream codefile(filename);
    return parse(codefile);
}
#endif
