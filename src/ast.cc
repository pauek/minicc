#include <assert.h>
#include <iostream>
#include <sstream>
using namespace std;
#include "ast.hh"
#include "translator.hh"

bool CommentSeq::has_endln() const {
    for (const Comment& c : comments) {
        if (c.kind == Comment::EndLine) {
            return true;
        }
    }
    return false;
}

bool CommentSeq::ends_with_empty_line() const {
    const int sz = comments.size();
    return sz >= 2 and (comments[sz - 2].kind == Comment::EndLine and
                        comments[sz - 1].kind == Comment::EndLine);
}

void CommentSeq::remove_endlns() {
    comments.erase(
        std::remove_if(
            comments.begin(), comments.end(), [](Comment& c) { return c.kind == Comment::EndLine; }
        ),
        comments.end()
    );
}

void CommentSeq::only_one_endln_at_end() {
    if (comments.empty() or comments.back().kind != Comment::EndLine) {
        return;
    }
    int i = comments.size() - 1;
    while (1) {
        if (comments[i - 1].kind != Comment::EndLine) {
            break;
        }
        i--;
    }
    comments.resize(i + 1);
}

void AstNode::add_error(string msg) {
    errors.push_back(new Error(span, msg));
}

void AstNode::add_error(Pos _ini, Pos _fin, string msg) {
    errors.push_back(new Error(Span(_ini, _fin), msg));
}

void Error::to_json(ostream& o) const {
    ostringstream oss;
    o << "{";
    o << "\"ini\": ";
    span.begin.to_json(o);
    o << ", ";
    o << "\"fin\": ";
    span.end.to_json(o);
    o << ", ";
    o << "\"msg\": \"" << msg << "\"";
    o << "}";
}

Expr::Kind Expr::TokenToKind(Token::Type tokkind) {
    switch (tokkind) {
        case Token::Empty:
            return Expr::Unknown;
        case Token::Comma:
            return Expr::Comma;
        case Token::Eq:
        case Token::PlusEq:
        case Token::MinusEq:
        case Token::StarEq:
        case Token::SlashEq:
        case Token::DivEq:
        case Token::LShiftEq:
        case Token::RShiftEq:
        case Token::AmpEq:
        case Token::BarEq:
        case Token::XorEq:
            return Expr::Eq;
        case Token::Colon:
            return Expr::Infinite;
        case Token::QMark:
            return Expr::Conditional;
        case Token::Or:
        case Token::BarBar:
            return Expr::LogicalOr;
        case Token::And:
        case Token::AmpAmp:
            return Expr::LogicalAnd;
        case Token::Bar:
            return Expr::BitOr;
        case Token::Xor:
            return Expr::BitXor;
        case Token::Amp:
            return Expr::BitAnd;
        case Token::EqEq:
        case Token::ExclEq:
            return Expr::Equality;
        case Token::LT:
        case Token::GT:
        case Token::GE:
        case Token::LE:
            return Expr::Relational;
        case Token::LShift:
        case Token::RShift:
            return Expr::Shift;
        case Token::Plus:
        case Token::Minus:
            return Expr::Additive;
        case Token::Star:
        case Token::Slash:
        case Token::Div:
            return Expr::Multiplicative;
        default:
            return Expr::Unknown;
    }
}

JumpStmt::Kind JumpStmt::KeywordToType(string s) {
    if (s == "break") {
        return JumpStmt::Break;
    } else if (s == "continue") {
        return JumpStmt::Continue;
    } else if (s == "goto") {
        return JumpStmt::Goto;
    } else {
        return JumpStmt::Unknown;
    }
}

string Literal::Escape(char c, char delim) {
    switch (c) {
        case '\a':
            return "\\a";
        case '\b':
            return "\\b";
        case '\f':
            return "\\f";
        case '\n':
            return "\\n";
        case '\r':
            return "\\r";
        case '\t':
            return "\\t";
        case '\v':
            return "\\v";
        case '\?':
            return "\\?";
        case '\\':
            return "\\\\";
        case '\"':
            return (c == delim ? "\\\"" : "\"");
        case '\'':
            return (c == delim ? "\\\'" : "\'");
        default:
            return string(1, c);
    }
}

string Literal::Escape(string s, char delim) {
    string r;
    for (char c : s) {
        r += Escape(c, delim);
    }
    return r;
}

string Identifier::TypeStr() const {
    string typestr;
    for (int i = 0; i < prefix.size(); i++) {
        typestr += prefix[i]->TypeStr();
        typestr += "::";
    }
    typestr += name;
    if (!subtypes.empty()) {
        typestr += "<";
        for (int i = 0; i < subtypes.size(); i++) {
            if (i > 0) {
                typestr += ",";
            }
            typestr += subtypes[i]->TypeStr();
        }
        typestr += ">";
    }
    return typestr;
}

Identifier *Identifier::GetPotentialNamespaceOrClass() const {
    if (prefix.size() == 1 and !prefix[0]->is_template()) {
        return prefix[0];
    }
    return 0;
}

vector<Identifier *> Identifier::GetNonNamespaces() {
    vector<Identifier *>::iterator it = prefix.begin();
    while (it != prefix.end() and (*it)->is_namespace) {
        it++;
    }
    vector<Identifier *> result(it, prefix.end());
    result.push_back(this);
    return result;
}

Identifier *TypeSpec::GetPotentialNamespaceOrClass() const {
    return id->GetPotentialNamespaceOrClass();
}

string TypeSpec::TypeStr() const {
    string typestr;
#define QUALIFIER(qual, str)  \
    if (HasQualifier(qual)) { \
        typestr += str;       \
        typestr += ' ';       \
    }
    QUALIFIER(Const, "const")
    QUALIFIER(Volatile, "volatile")
    QUALIFIER(Mutable, "mutable")
    QUALIFIER(Register, "register")
    QUALIFIER(Auto, "auto")
    QUALIFIER(Extern, "extern")
#undef QUALIFIER
    typestr += id->TypeStr();
    if (reference) {
        typestr += "&";
    }
    return typestr;
}

string ArrayDecl::TypeStr() const {
    string brackets;
    for (int i = 0; i < sizes.size(); i++) {
        brackets += "[]";
    }
    return typespec->TypeStr() + brackets;
}

string StructDecl::TypeStr() const {
    string typestr;
    typestr += "struct{";
    for (int i = 0; i < decls.size(); i++) {
        if (i > 0) {
            typestr += ";";
        }
        typestr += decls[i]->typespec->TypeStr();
    }
    typestr += "}";
    return typestr;
}

void Identifier::Shift(string new_id) {
    Identifier *pre = new Identifier();
    pre->name = name;
    pre->subtypes.swap(subtypes);
    pre->comments.swap(comments);
    pre->errors.swap(errors);
    // the last two comments are the ones surrounding the "::",
    // copy them over to new_id
    const int commsz = pre->comments.size();
    comments.push_back(pre->comments[commsz - 2]);
    comments.push_back(pre->comments[commsz - 1]);
    pre->comments.resize(commsz - 2);
    prefix.push_back(pre);
    name = new_id;
}

void collect_rights(AstNode *node, list<Expr *>& L) {
    if (is_a<BinaryExpr>(node)) {
        BinaryExpr *X = cast<BinaryExpr>(node);
        L.push_front(X->right);
        collect_rights(X->left, L);
    }
}

bool is_read_expr(AstNode *ast) {
    switch (ast->type()) {
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(ast);
            if (is_a<Identifier>(X->left)) {
                Identifier *id = cast<Identifier>(X->left);
                return id->name == "cin";
            } else {
                return is_read_expr(X->left) and X->op == ">>";
            }
        }
        default:
            return false;
    }
}

bool is_write_expr(AstNode *ast) {
    switch (ast->type()) {
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(ast);
            if (is_a<Identifier>(X->left)) {
                Identifier *id = cast<Identifier>(X->left);
                return id->name == "cout";
            } else {
                return is_write_expr(X->left) and X->op == "<<";
            }
        }
        default:
            return false;
    }
}

bool is_assignment(AstNode *node) {
    if (is_a<BinaryExpr>(node)) {
        BinaryExpr *X = cast<BinaryExpr>(node);
        return X->kind == Expr::Eq;
    }
    return false;
}

string describe(AstNode *node) {
    switch (node->type()) {
        case AstNodeType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(node);
            return describe(X->expr);
        }
        case AstNodeType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(node);
            if (is_a<Identifier>(X->expr)) {
                Identifier *id = cast<Identifier>(X->expr);
                return _T("Se incrementa la variable '%s'.", id->name.c_str());
            }
            return _T("UNIMPLEMENTED");
        }
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(node);
            if (is_write_expr(X)) {
                return _T("Some output is written.");
            }
            if (is_read_expr(X)) {
                return _T("Some input is read.");
            }
            return _T("UNIMPLEMENTED");
        }
        case AstNodeType::DeclStmt: {
            DeclStmt *X = cast<DeclStmt>(node);
            if (X->items.size() == 1) {
                return _T("Se declara la variable '%s'.", X->items[0].decl->name.c_str());
            }
            ostringstream S;
            for (int i = 0; i < X->items.size(); i++) {
                if (i > 0) {
                    if (i == X->items.size() - 1) {
                        S << _T(" and ");
                    } else {
                        S << ", ";
                    }
                }
                S << "'" << X->items[i].decl->name << "'";
            }
            return _T("Variables %s are declared.", S.str().c_str());
        }
        default:
            return _T("UNIMPLEMENTED");
    }
}

bool has_errors(AstNode *node) {
#define CHECK_ERRORS(n) \
    if (has_errors(n))  \
        return true;

    if (node == 0) {
        return false;
    }
    switch (node->type()) {
        case AstNodeType::Program: {
            Program *X = cast<Program>(node);
            for (AstNode *n : X->nodes) {
                if (has_errors(n)) {
                    return true;
                }
            }
            return X->has_errors();
        }
        case AstNodeType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        case AstNodeType::IfStmt: {
            IfStmt *X = cast<IfStmt>(node);
            CHECK_ERRORS(X->cond);
            CHECK_ERRORS(X->then);
            CHECK_ERRORS(X->els);
            return X->has_errors();
        }
        case AstNodeType::ForStmt: {
            ForStmt *X = cast<ForStmt>(node);
            CHECK_ERRORS(X->init);
            CHECK_ERRORS(X->cond);
            CHECK_ERRORS(X->post);
            CHECK_ERRORS(X->substmt);
            return X->has_errors();
        }
        case AstNodeType::WhileStmt: {
            WhileStmt *X = cast<WhileStmt>(node);
            CHECK_ERRORS(X->cond);
            CHECK_ERRORS(X->substmt);
            return X->has_errors();
        }
        case AstNodeType::DeclStmt: {
            DeclStmt *X = cast<DeclStmt>(node);
            CHECK_ERRORS(X->typespec);
            for (DeclStmt::Item i : X->items) {
                CHECK_ERRORS(i.decl);
                CHECK_ERRORS(i.init);
            }
            return X->has_errors();
        }
        case AstNodeType::Block: {
            Block *X = cast<Block>(node);
            for (Stmt *s : X->stmts) {
                CHECK_ERRORS(s);
            }
            return X->has_errors();
        }
        case AstNodeType::Identifier: {
            Identifier *X = cast<Identifier>(node);
            for (Identifier *id : X->prefix) {
                CHECK_ERRORS(id);
            }
            for (TypeSpec *t : X->subtypes) {
                CHECK_ERRORS(t);
            }
            return X->has_errors();
        }
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(node);
            CHECK_ERRORS(X->left);
            CHECK_ERRORS(X->right);
            return X->has_errors();
        }
        case AstNodeType::CallExpr: {
            CallExpr *X = cast<CallExpr>(node);
            CHECK_ERRORS(X->func);
            return X->has_errors();
        }
        case AstNodeType::IndexExpr: {
            IndexExpr *X = cast<IndexExpr>(node);
            CHECK_ERRORS(X->base);
            CHECK_ERRORS(X->index);
            return X->has_errors();
        }
        case AstNodeType::FieldExpr: {
            FieldExpr *X = cast<FieldExpr>(node);
            CHECK_ERRORS(X->base);
            return X->has_errors();
        }
        case AstNodeType::CondExpr: {
            CondExpr *X = cast<CondExpr>(node);
            CHECK_ERRORS(X->cond);
            CHECK_ERRORS(X->then);
            CHECK_ERRORS(X->els);
            return X->has_errors();
        }
        case AstNodeType::ExprList: {
            ExprList *X = cast<ExprList>(node);
            for (Expr *e : X->exprs) {
                CHECK_ERRORS(e);
            }
            return X->has_errors();
        }
        case AstNodeType::TypeSpec: {
            TypeSpec *X = cast<TypeSpec>(node);
            CHECK_ERRORS(X->id);
            return X->has_errors();
        }
        case AstNodeType::FuncDecl: {
            FuncDecl *X = cast<FuncDecl>(node);
            CHECK_ERRORS(X->return_typespec);
            CHECK_ERRORS(X->block);
            for (FuncDecl::Param *p : X->params) {
                CHECK_ERRORS(p->typespec);
            }
            return X->has_errors();
        }
        case AstNodeType::StructDecl: {
            StructDecl *X = cast<StructDecl>(node);
            for (DeclStmt *d : X->decls) {
                CHECK_ERRORS(d);
            }
            return X->has_errors();
        }
        case AstNodeType::TypedefDecl: {
            TypedefDecl *X = cast<TypedefDecl>(node);
            CHECK_ERRORS(X->decl);
            return X->has_errors();
        }
        case AstNodeType::SignExpr: {
            SignExpr *X = cast<SignExpr>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        case AstNodeType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        case AstNodeType::NegExpr: {
            NegExpr *X = cast<NegExpr>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        case AstNodeType::AddrExpr: {
            AddrExpr *X = cast<AddrExpr>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        case AstNodeType::DerefExpr: {
            DerefExpr *X = cast<DerefExpr>(node);
            CHECK_ERRORS(X->expr);
            return X->has_errors();
        }
        default:
            return node->has_errors();
    }

#undef CHECK_ERRORS
}
