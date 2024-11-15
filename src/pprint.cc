
#include "pprint.hh"
#include <algorithm>
#include <sstream>
#include "ast.hh"
using namespace std;

class OutputWriter {
    int      indent_;
    ostream& out_;

   public:
    OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

    void indent() { indent_ += 3; }

    void dedent() { indent_ -= 3; }

    void indentation() { out_ << std::string(indent_, ' '); }

    template <typename T>
    void write(const T& t) {
        out_ << t;
    }

    template <typename T1, typename T2>
    void write(const T1& t1, const T2& t2) {
        out_ << t1 << t2;
    }

    template <typename T1, typename T2, typename T3>
    void write(const T1& t1, const T2& t2, const T3& t3) {
        out_ << t1 << t2 << t3;
    }

    template <typename T1, typename T2, typename T3, typename T4>
    void write(const T1& t1, const T2& t2, const T3& t3, const T4& t4) {
        out_ << t1 << t2 << t3 << t4;
    }

    template <typename T1, typename T2, typename T3, typename T4, typename T5>
    void write(const T1& t1, const T2& t2, const T3& t3, const T4& t4, const T5& t5) {
        out_ << t1 << t2 << t3 << t4 < t5;
    }

    void endln() { out_ << endl; }

    void beginln() { out_ << string(indent_, ' '); }
};

class CmtPr {  // Comment Printer
    OutputWriter& out_;
    AstNode      *ast_;
    int           index_;
    bool          was_empty_, had_endl_;
    void          _write_comment(bool pre, bool post, bool _endl);

   public:
    CmtPr(AstNode *node, OutputWriter& out)
        : ast_(node), out_(out), index_(0), had_endl_(false), was_empty_(true) {}

    CommentSeq *Next() const {
        return (index_ < ast_->comments.size() ? ast_->comments[index_] : 0);
    }

    bool last_had_endln() const { return had_endl_; }

    bool last_was_empty() const { return was_empty_; }

    void space_comment() { return _write_comment(1, 0, 0); }

    void space_comment_space() { return _write_comment(1, 1, 0); }

    void comment_space() { return _write_comment(0, 1, 0); }

    void comment() { return _write_comment(0, 0, 0); }

    void space_comment_endln() { return _write_comment(1, 0, 1); }
};

void CmtPr::_write_comment(bool pre, bool post, bool _endl) {
    CommentSeq *C = 0;
    if (index_ < ast_->comments.size()) {
        C = ast_->comments[index_];
    }
    was_empty_ = (C == 0);
    had_endl_ = (C != 0 ? C->has_endln() : false);
    index_++;
    if (C != 0 and !C->comments.empty()) {
        if (pre and !C->starts_with_endln()) {
            out_.write(' ');
        }
        if (C != 0) {
            for (int i = 0; i < C->comments.size(); i++) {
                const ::Comment& c = C->comments[i];  // UGLY
                switch (c.kind) {
                    case Comment::None:
                        break;
                    case Comment::MultiLine:
                        if (i > 0 and C->comments[i - 1].kind != Comment::EndLine) {
                            out_.write(' ');
                        }
                        out_.write("/*", c.text, "*/");
                        break;
                    case Comment::SingleLine:
                        out_.write("//", c.text);
                        break;
                    case Comment::EndLine:
                        out_.endln();
                        out_.beginln();
                        break;
                }
            }
        }
        if (_endl and !C->has_endln()) {
            out_.endln();
            out_.beginln();
        } else if (!_endl and post) {
            out_.write(' ');
        }
    } else if (_endl) {
        out_.endln();
        out_.beginln();
    }
}

struct PrettyPrinter {
    OutputWriter out;

    PrettyPrinter(ostream& o) : out(o) {}

    void Print(AstNode *node);
};

void PrettyPrinter::Print(AstNode *node) {
    assert(node != nullptr);
    switch (node->type()) {
        case AstNodeType::Program: {
            auto *X = cast<Program>(node);
            CmtPr cp(X, out);
            for (int i = 0; i < X->nodes.size(); i++) {
                cp.comment();
                AstNode *n = X->nodes[i];
                if ((!cp.last_was_empty() and !cp.last_had_endln()) or
                    (i > 0 and is_a<FuncDecl>(n)) and
                        (X->comments[i] and !X->comments[i]->ends_with_empty_line())) {
                    out.endln();
                }
                Print(n);
                if (cp.Next() and !cp.Next()->starts_with_endln()) {
                    out.write(' ');
                }
            }
            CommentSeq *last = cp.Next();
            if (last) {
                last->only_one_endln_at_end();
            }
            cp.comment();
            if (last == 0 or !last->has_endln()) {
                out.endln();
            }
            break;
        }
        case AstNodeType::Include: {
            auto  *X = cast<Include>(node);
            CmtPr  cp(X, out);
            string delim = (X->global ? "<>" : "\"\"");
            out.write("#include ");
            cp.comment_space();
            out.write(delim[0], X->filename, delim[1]);
            break;
        }
        case AstNodeType::Macro: {
            auto *X = cast<Macro>(node);
            out.write("#", X->macro);
            break;
        }
        case AstNodeType::Using: {
            auto *X = cast<Using>(node);
            CmtPr cp(X, out);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write("using ");
            cp.comment_space();
            out.write("namespace ");
            cp.comment_space();
            out.write(X->namespc);
            cp.space_comment();
            out.write(";");
            cp.comment();
            break;
        }
        case AstNodeType::TypeSpec: {
            auto *X = cast<TypeSpec>(node);
            CmtPr cp(X, out);
#define QUALIFIER(qual, str)               \
    if (X->HasQualifier(TypeSpec::qual)) { \
        out.write(str, " ");               \
        cp.comment_space();                \
    }
            QUALIFIER(Const, "const")
            QUALIFIER(Volatile, "volatile")
            QUALIFIER(Mutable, "mutable")
            QUALIFIER(Register, "register")
            QUALIFIER(Auto, "auto")
            QUALIFIER(Extern, "extern")
#undef QUALIFIER
            Print(X->id);
            if (X->reference) {
                cp.space_comment_space();
                out.write("&");
            }
            break;
        }
        case AstNodeType::EnumDecl: {
            auto *X = cast<EnumDecl>(node);
            CmtPr cp(X, out);
            out.write("enum ");
            cp.comment_space();
            out.write(X->name);
            cp.space_comment();
            out.write(" { ");
            cp.comment_space();
            for (int i = 0; i < X->values.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                    cp.comment_space();
                }
                out.write(X->values[i].id);
                cp.space_comment();
                if (X->values[i].has_val) {
                    out.write(" = ");
                    cp.comment_space();
                    out.write(X->values[i].val);
                    cp.space_comment();
                }
            }
            out.write(" };");
            break;
        }
        case AstNodeType::TypedefDecl: {
            auto *X = cast<TypedefDecl>(node);
            CmtPr cp(X, out);
            out.write("typedef ");
            cp.comment_space();
            Print(X->decl->typespec);
            out.write(" ");
            cp.comment_space();
            Print(X->decl);
            out.write(";");
            cp.space_comment();
            break;
        }
        case AstNodeType::StructDecl: {
            auto *X = cast<StructDecl>(node);
            CmtPr cp(X, out);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write("struct ");
            cp.comment_space();
            out.write(X->name);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            cp.space_comment();
            out.write(" {");
            out.indent();
            vector<string> decl_strings;
            // TODO: Alinear comentarios y nombres de variable verticalmente!
            for (DeclStmt *decl : X->decls) {
                cp.space_comment_endln();
                Print(decl);
            }
            out.dedent();
            // WARNING: g++ here optimizes and changes order of instructions!!!
            cp.space_comment_endln();
            out.write("}");
            cp.space_comment();
            out.write(";");
            break;
        }
        case AstNodeType::FuncDecl: {
            auto *X = cast<FuncDecl>(node);
            CmtPr cp(X, out);
            Print(X->return_typespec);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write(" ");
            cp.comment_space();
            Print(X->id);
            cp.space_comment_space();
            if (X->params.empty()) {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                out.write("(");
                cp.comment();
                out.write(")");
            } else {
                out.write("(");
                for (int i = 0; i < X->params.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    cp.comment_space();
                    Print(X->params[i]->typespec);
                    // WARNING: g++ here optimizes and changes order of instructions!!!
                    out.write(" ");
                    cp.comment_space();
                    out.write(X->params[i]->name);
                    cp.space_comment();
                }
                out.write(")");
            }
            if (cp.Next()) {
                cp.Next()->remove_endlns();
            }
            if (X->block) {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                out.write(" ");
                cp.comment_space();
                Print(X->block);
            } else {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                cp.space_comment();
                out.write(";");
            }
            break;
        }
        case AstNodeType::Block: {
            auto *X = cast<Block>(node);
            CmtPr cp(X, out);
            if (X->stmts.empty()) {
                out.write("{");
                if (cp.Next() and cp.Next()->has_endln()) {
                    cp.space_comment();
                } else {
                    cp.space_comment_space();
                }
                out.write("}");
                return;
            }
            out.indent();
            out.write("{");
            for (Stmt *stmt : X->stmts) {
                cp.space_comment_endln();
                Print(stmt);
            }
            out.dedent();
            cp.space_comment_endln();
            out.write("}");
            cp.space_comment();
            break;
        }
        case AstNodeType::Identifier: {
            auto *X = cast<Identifier>(node);
            CmtPr cp(X, out);
            for (Identifier *pre : X->prefix) {
                Print(pre);
                // WARNING: g++ here optimizes and changes order of instructions!!!
                cp.space_comment_space();
                out.write("::");
                cp.space_comment_space();
            }
            out.write(X->name);
            if (!X->subtypes.empty()) {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                cp.space_comment_space();
                out.write("<");
                cp.comment_space();
                for (int i = 0; i < X->subtypes.size(); i++) {
                    if (i > 0) {
                        // WARNING: g++ here optimizes and changes order of instructions!!!
                        out.write(", ");
                        cp.comment_space();
                    }
                    Print(X->subtypes[i]);
                    cp.space_comment();
                }
                out.write(">");
            }
            break;
        }
        case AstNodeType::Literal: {
            auto *X = cast<Literal>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            switch (X->kind) {
                case Literal::Bool:
                    out.write(X->val.as_bool ? "true" : "false");
                    break;
                case Literal::Int:
                    out.write(X->val.as_int);
                    break;
                case Literal::Double:
                    out.write(X->val.as_double);
                    break;
                case Literal::String:
                    out.write('"', Literal::Escape(*X->val.as_string.s, '"'), '"');
                    break;
                case Literal::Char: {
                    out.write("'", Literal::Escape(X->val.as_char, '\''), "'");
                    break;
                }
                default:
                    out.write("<literal>");
                    break;
            }
            cp.space_comment();
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            auto *X = cast<BinaryExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                out.write("(");
                cp.comment_space();
            }
            Print(X->left);
            if (X->op != ",") {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                cp.space_comment();
                out.write(" ");
            }
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write(X->op, " ");
            cp.comment_space();
            Print(X->right);
            if (X->paren) {
                cp.space_comment();
                out.write(")");
            }
            cp.space_comment();
            break;
        }
        case AstNodeType::VarDecl: {
            auto *X = cast<VarDecl>(node);
            CmtPr cp(X, out);
            if (X->kind == Decl::Pointer) {
                out.write("*");
            }
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write(X->name);
            cp.space_comment();
            break;
        }
        case AstNodeType::ExprList: {
            auto *X = cast<ExprList>(node);
            CmtPr cp(X, out);
            out.write("{");
            for (int i = 0; i < X->exprs.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                Print(X->exprs[i]);
            }
            out.write("}");
            break;
        }
        case AstNodeType::ArrayDecl: {
            auto *X = cast<ArrayDecl>(node);
            CmtPr cp(X, out);
            out.write(X->name);
            cp.space_comment_space();
            for (int i = 0; i < X->sizes.size(); i++) {
                out.write("[");
                cp.comment_space();
                Print(X->sizes[i]);
                out.write("]");
                cp.space_comment();
            }
            break;
        }
        case AstNodeType::ObjDecl: {
            auto *X = cast<ObjDecl>(node);
            CmtPr cp(X, out);
            out.write(X->name);
            cp.space_comment();
            if (!X->args.empty()) {
                out.write("(");
                for (int i = 0; i < X->args.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    cp.comment_space();
                    Print(X->args[i]);
                }
                out.write(")");
            }
            break;
        }
        case AstNodeType::DeclStmt: {
            auto *X = cast<DeclStmt>(node);
            CmtPr cp(X, out);
            Print(X->typespec);
            out.write(" ");
            cp.comment_space();
            for (int i = 0; i < X->items.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                    cp.comment_space();
                }
                DeclStmt::Item& item = X->items[i];
                Print(item.decl);
                if (item.init) {
                    out.write(" = ");
                    cp.comment_space();
                    Print(item.init);
                }
            }
            out.write(";");
            break;
        }
        case AstNodeType::ExprStmt: {
            auto *X = cast<ExprStmt>(node);
            CmtPr cp(X, out);
            if (X->is_return) {
                out.write("return ");
                cp.comment_space();
            }
            if (X->expr) {
                Print(X->expr);
            }
            cp.space_comment();
            out.write(";");
            break;
        }
        case AstNodeType::IfStmt: {
            auto *X = cast<IfStmt>(node);
            CmtPr cp(X, out);
            out.write("if ");
            cp.comment_space();
            out.write("(");
            cp.comment_space();
            Print(X->cond);
            out.write(") ");
            cp.comment_space();
            Print(X->then);
            if (X->els) {
                cp.space_comment();
                if (!cp.last_had_endln()) {
                    out.write(" ");
                }
                out.write("else ");
                cp.comment_space();
                Print(X->els);
            }
            break;
        }
        case AstNodeType::ForStmt: {
            auto *X = cast<ForStmt>(node);
            CmtPr cp(X, out);
            out.write("for ");
            cp.comment_space();
            out.write("(");
            if (X->init) {
                Print(X->init);
            }
            out.write(" ");
            if (X->cond) {
                Print(X->cond);
            }
            out.write("; ");
            if (X->post) {
                Print(X->post);
            }
            out.write(")");
            cp.space_comment();
            if (!cp.last_had_endln()) {
                out.write(" ");
            }
            if (not is_a<Block>(X->substmt) and cp.last_had_endln()) {
                out.indentation();
            }
            Print(X->substmt);
            break;
        }
        case AstNodeType::WhileStmt: {
            auto *X = cast<WhileStmt>(node);
            CmtPr cp(X, out);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write("while ");
            cp.comment_space();
            out.write("(");
            cp.comment_space();
            Print(X->cond);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            cp.space_comment();
            out.write(")");
            cp.space_comment();
            if (!cp.last_had_endln()) {
                out.write(" ");
            }
            if (not is_a<Block>(X->substmt) and cp.last_had_endln()) {
                out.indentation();
            }
            Print(X->substmt);
            break;
        }
        case AstNodeType::JumpStmt: {
            auto  *X = cast<JumpStmt>(node);
            CmtPr  cp(X, out);
            string keyword[3] = {"break", "continue", "goto"};
            out.write(keyword[X->kind]);
            cp.space_comment();
            if (X->kind == JumpStmt::Goto) {
                // WARNING: g++ here optimizes and changes order of instructions!!!
                out.write(" ", X->label);
                cp.space_comment();
            }
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write(";");
            cp.space_comment();
            break;
        }
        case AstNodeType::CallExpr: {
            auto *X = cast<CallExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            Print(X->func);
            if (cp.Next() and cp.Next()->ends_with_endln()) {
                out.write(" ");
            }
            cp.space_comment_space();
            out.write("(");
            for (int i = 0; i < X->args.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                    cp.comment_space();
                }
                cp.comment_space();
                Print(X->args[i]);
            }
            out.write(")");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::IndexExpr: {
            auto *X = cast<IndexExpr>(node);
            if (X->paren) {
                out.write("(");
            }
            Print(X->base);
            out.write("[");
            Print(X->index);
            out.write("]");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::FieldExpr: {
            auto *X = cast<FieldExpr>(node);
            if (X->paren) {
                out.write("(");
            }
            Print(X->base);
            out.write(X->pointer ? "->" : ".", X->field);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::CondExpr: {
            auto *X = cast<CondExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            Print(X->cond);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            cp.space_comment();
            out.write(" ? ");
            cp.comment_space();
            Print(X->then);
            // WARNING: g++ here optimizes and changes order of instructions!!!
            cp.space_comment();
            out.write(" : ");
            cp.comment_space();
            Print(X->els);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::SignExpr: {
            auto *X = cast<SignExpr>(node);
            if (X->paren) {
                out.write("(");
            }
            out.write(X->kind == SignExpr::Positive ? "+" : "-");
            Print(X->expr);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::IncrExpr: {
            auto *X = cast<IncrExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            if (X->preincr) {
                out.write(X->kind == IncrExpr::Positive ? "++" : "--");
                cp.space_comment_space();
                Print(X->expr);
            } else {
                Print(X->expr);
                out.write(X->kind == IncrExpr::Positive ? "++" : "--");
                cp.space_comment_space();
            }
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::NegExpr: {
            auto *X = cast<NegExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            out.write("!");
            cp.space_comment_space();
            Print(X->expr);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::AddrExpr: {
            auto *X = cast<AddrExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
                cp.comment_space();
            }
            // WARNING: g++ here optimizes and changes order of instructions!!!
            out.write("&");
            cp.space_comment_space();
            Print(X->expr);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::DerefExpr: {
            auto *X = cast<DerefExpr>(node);
            CmtPr cp(X, out);
            if (X->paren) {
                out.write("(");
            }
            out.write("*");
            cp.space_comment_space();
            Print(X->expr);
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        default:
            out.write("<error>");
            break;
    }
}

void pretty_print(AstNode *node, ostream& out) {
    PrettyPrinter(out).Print(node);
}

string pretty_to_string(AstNode *node) {
    stringstream ss;
    pretty_print(node, ss);
    return ss.str();
}
