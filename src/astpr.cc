#include "astpr.hh"
#include <algorithm>
#include <sstream>
#include "ast.hh"
using namespace std;

struct OutputWriter {
    OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

    void indent() { indent_ += 3; }

    void dedent() { indent_ -= 3; }

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

    void endln() { out_ << endl; }

    void beginln() { out_ << string(indent_, ' '); }

    void Line(string s) {
        beginln();
        write(s);
        endln();
    }

   private:
    int      indent_;
    ostream& out_;
};

struct AstPrinter {
    OutputWriter out;

    AstPrinter(ostream& o) : out(o) {}

    void print(AstNodeCore *ast);
};

void AstPrinter::print(AstNodeCore *node) {
    assert(node != nullptr);
    switch (node->type()) {
        case AstNodeType::Program: {
            auto *X = cast<Program>(node);
            out.Line("Program{");
            out.indent();
            for (AstNodeCore *child : X->nodes) {
                out.beginln();
                print(child);
                out.endln();
            }
            out.dedent();
            out.Line("}");
            break;
        }
        case AstNodeType::Include: {
            auto  *X = cast<Include>(node);
            string D = (X->global ? "<>" : "\"\"");
            out.write("Include(");
            out.write(D[0], X->filename, D[1]);
            out.write(")");
            break;
        }
        case AstNodeType::Macro: {
            auto *X = cast<Macro>(node);
            out.write("Macro(", X->macro, ")");
            break;
        }
        case AstNodeType::Using: {
            auto *X = cast<Using>(node);
            out.write("Using(", X->namespc, ")");
            break;
        }
        case AstNodeType::TypeSpec: {
            auto *X = cast<TypeSpec>(node);
            out.write("Type");
            if (X->reference) {
                out.write("<&>");
            }
            out.write("(");
            print(X->id);
            if (X->has_qualifiers()) {
                int count = 0;
                out.write(", {");
#define QUALIFIER(qual, str)         \
    if (X->bqual & TypeSpec::qual) { \
        if (count > 0)               \
            out.write(", ");         \
        out.write(str);              \
        count++;                     \
    }
                QUALIFIER(Const, "const")
                QUALIFIER(Volatile, "volatile")
                QUALIFIER(Mutable, "mutable")
                QUALIFIER(Register, "register")
                QUALIFIER(Auto, "auto")
                QUALIFIER(Extern, "extern")
#undef QUALIFIER
                out.write("}");
            }
            out.write(")");
            break;
        }
        case AstNodeType::EnumDecl: {
            auto *X = cast<EnumDecl>(node);
            out.write("EnumDecl(\"", X->name, "\", {");
            for (int i = 0; i < X->values.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                out.write('"', X->values[i].id, '"');
                if (X->values[i].has_val) {
                    out.write(" = ", X->values[i].val);
                }
            }
            out.write("})");
            break;
        }
        case AstNodeType::TypedefDecl: {
            auto *X = cast<TypedefDecl>(node);
            out.write("TypedefDecl(\"", X->decl->name, "\" = ");
            print(X->decl->typespec);
            out.write(")");
            break;
        }
        case AstNodeType::StructDecl: {
            auto *X = cast<StructDecl>(node);
            out.write("StructDecl('", X->name, "', {");
            out.endln();
            out.indent();
            for (DeclStmt *decl : X->decls) {
                out.beginln();
                print(decl);
                out.endln();
            }
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::FuncDecl: {
            auto *X = cast<FuncDecl>(node);
            out.write("FuncDecl(");
            print(X->id);
            out.write(", ");
            print(X->return_typespec);
            out.write(", Params = {");
            for (int i = 0; i < X->params.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                out.write('"', X->params[i]->name, "\": ");
                print(X->params[i]->typespec);
            }
            if (X->block) {
                out.write("}, {");
                out.endln();
                out.indent();
                out.beginln();
                print(X->block);
                out.endln();
                out.dedent();
                out.beginln();
            }
            out.write("})");
            break;
        }
        case AstNodeType::Block: {
            auto *X = cast<Block>(node);
            out.write("Block(");
            if (X->stmts.empty()) {
                out.write("{})");
                break;
            }
            out.write("{");
            out.endln();
            out.indent();
            for (Stmt *stmt : X->stmts) {
                out.beginln();
                print(stmt);
                out.endln();
            }
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::Identifier: {
            auto *X = cast<Identifier>(node);
            out.write("id:");
            if (!X->prefix.empty()) {
                out.write("[");
                for (int i = 0; i < X->prefix.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    print(X->prefix[i]);
                }
                out.write("]");
            }
            out.write("'", X->name, "'");
            if (!X->subtypes.empty()) {
                out.write("<");
                for (int i = 0; i < X->subtypes.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    print(X->subtypes[i]);
                }
                out.write(">");
            }
            break;
        }
        case AstNodeType::Literal: {
            auto *X = cast<Literal>(node);
            if (X->paren) {
                out.write("(");
            }
            switch (X->kind) {
                case Literal::Bool:
                    out.write("Bool<", X->val.as_bool ? "true" : "false", ">");
                    break;
                case Literal::Char:
                    out.write("Char<", Literal::escape(X->val.as_char, '\''), ">");
                    break;
                case Literal::Int:
                    out.write("Int<", X->val.as_int, ">");
                    break;
                case Literal::Float:
                    out.write("Float<", X->val.as_double, ">");
                    break;
                case Literal::Double:
                    out.write("Double<", X->val.as_double, ">");
                    break;
                case Literal::String:
                    out.write("String<", Literal::escape(*(X->val.as_string.s), '"'), ">");
                    break;
                default:
                    out.write("Literal<>");
                    break;
            }
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            auto *X = cast<BinaryExpr>(node);
            if (X->paren) {
                out.write("(");
            }
            out.write(X->op, "(");
            print(X->left);
            out.write(", ");
            print(X->right);
            out.write(")");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::VarDecl: {
            auto *X = cast<VarDecl>(node);
            if (X->kind == Decl::Pointer) {
                out.write("*");
            }
            out.write('"', X->name, '"');
            break;
        }
        case AstNodeType::ArrayDecl: {
            auto *X = cast<ArrayDecl>(node);
            out.write('"', X->name, "\"(");
            if (X->sizes.size() == 1) {
                out.write("Size = ");
                print(X->sizes[0]);
            } else {
                out.write("Sizes = {");
                for (int i = 0; i < X->sizes.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    print(X->sizes[i]);
                }
                out.write("}");
            }
            out.write(")");
            break;
        }
        case AstNodeType::ExprList: {
            auto *X = cast<ExprList>(node);
            out.write("{");
            for (int i = 0; i < X->exprs.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                print(X->exprs[i]);
            }
            out.write("}");
            break;
        }
        case AstNodeType::ObjDecl: {
            auto *X = cast<ObjDecl>(node);
            out.write('"', X->name, "\"(");
            if (!X->args.empty()) {
                out.write("Args = {");
                for (int i = 0; i < X->args.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    print(X->args[i]);
                }
                out.write("}");
            }
            out.write(")");
            break;
        }
        case AstNodeType::DeclStmt: {
            auto *X = cast<DeclStmt>(node);
            out.write("DeclStmt(");
            print(X->typespec);
            out.write(", Vars = {");
            for (int i = 0; i < X->items.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                print(X->items[i].decl);
                if (X->items[i].init) {
                    out.write(" = ");
                    print(X->items[i].init);
                }
            }
            out.write("})");
            break;
        }
        case AstNodeType::ExprStmt: {
            auto *X = cast<ExprStmt>(node);
            out.write("ExprStmt");
            if (X->is_return) {
                out.write("<return>");
            }
            out.write("(");
            if (X->expr) {
                print(X->expr);
            }
            out.write(")");
            break;
        }
        case AstNodeType::IfStmt: {
            auto *X = cast<IfStmt>(node);
            out.write("IfStmt(");
            print(X->cond);
            out.write(", ");
            print(X->then);
            if (X->els) {
                out.write(", ");
                print(X->els);
            }
            out.write(")");
            break;
        }
        case AstNodeType::ForStmt: {
            auto *X = cast<ForStmt>(node);
            out.write("ForStmt(");
            if (X->init) {
                print(X->init);
            } else {
                out.write("_");
            }
            out.write(", ");
            if (X->cond) {
                print(X->cond);
            } else {
                out.write("_");
            }
            out.write(", ");
            if (X->post) {
                print(X->post);
            } else {
                out.write("_");
            }
            out.write(", {");
            out.endln();
            out.indent();
            out.beginln();
            print(X->substmt);
            out.endln();
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::WhileStmt: {
            auto *X = cast<WhileStmt>(node);
            out.write("WhileStmt(");
            print(X->cond);
            out.write(", {");
            out.endln();
            out.indent();
            out.beginln();
            print(X->substmt);
            out.endln();
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::JumpStmt: {
            auto         *X = cast<JumpStmt>(node);
            static string keyword[] = {"break", "continue", "goto"};
            out.write("JumpStmt<", keyword[X->kind], ">(");
            if (X->kind == JumpStmt::Goto) {
                out.write('"', X->label, '"');
            }
            out.write(")");
            break;
        }
        case AstNodeType::CallExpr: {
            auto *X = cast<CallExpr>(node);
            out.write("CallExpr(");
            print(X->func);
            out.write(", Args = {");
            for (int i = 0; i < X->args.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                print(X->args[i]);
            }
            out.write("})");
            break;
        }
        case AstNodeType::IndexExpr: {
            auto *X = cast<IndexExpr>(node);
            out.write("IndexExpr(");
            print(X->base);
            out.write(", ");
            print(X->index);
            out.write(")");
            break;
        }
        case AstNodeType::FieldExpr: {
            auto *X = cast<FieldExpr>(node);
            out.write("FieldExpr");
            if (X->pointer) {
                out.write("<pointer>");
            }
            out.write("(");
            print(X->base);
            out.write(", '", X->field, "')");
            break;
        }
        case AstNodeType::CondExpr: {
            auto *X = cast<CondExpr>(node);
            if (X->paren) {
                out.write("(");
            }
            out.write("CondExpr(");
            print(X->cond);
            out.write(", ");
            print(X->then);
            out.write(", ");
            print(X->els);
            out.write(")");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::SignExpr: {
            auto *X = cast<SignExpr>(node);
            out.write("SignExpr<", (X->kind == SignExpr::Positive ? '+' : '-'), ">(");
            print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::NegExpr: {
            auto *X = cast<NegExpr>(node);
            out.write("NegExpr(");
            print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::IncrExpr: {
            auto *X = cast<IncrExpr>(node);
            out.write("IncrExpr<", X->kind == IncrExpr::Positive ? "++" : "--");
            out.write(", ", (X->preincr ? "pre" : "post"), ">(");
            print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::AddrExpr: {
            auto *X = cast<AddrExpr>(node);
            out.write("AddrExpr(");
            print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::DerefExpr: {
            auto *X = cast<DerefExpr>(node);
            out.write("DerefExpr(");
            print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::StmtError: {
            auto *X = cast<StmtError>(node);
            out.write("ErrorStmt(\"", X->code, "\")");
            break;
        }
        case AstNodeType::ExprError: {
            auto *X = cast<ExprError>(node);
            out.write("ErrorExpr(\"", X->code, "\")");
            break;
        }
        default:
            out.write("<error>");
            break;
    }
}

void ast_print(AstNodeCore *ast, ostream& out) {
    AstPrinter(out).print(ast);
}
