#include "astpr.hh"
#include <algorithm>
#include <sstream>
#include "ast.hh"
using namespace std;

struct OutputWriter {
    OutputWriter(ostream& out = std::cout) : indent_(0), out_(out) {}

    void indent() { indent_ += 3; }

    void dedent() { indent_ -= 3; }

    template <typename TestClass>
    void write(const TestClass& t) {
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

    void Print(AstNode *ast);
};

void AstPrinter::Print(AstNode *ast) {
    assert(ast != nullptr);
    switch (ast->type()) {
        case AstNodeType::Program: {
            auto *X = cast<Program>(ast);
            out.Line("Program{");
            out.indent();
            for (AstNode *child : X->nodes) {
                out.beginln();
                Print(child);
                out.endln();
            }
            out.dedent();
            out.Line("}");
            break;
        }
        case AstNodeType::Include: {
            auto  *X = cast<Include>(ast);
            string D = (X->global ? "<>" : "\"\"");
            out.write("Include(");
            out.write(D[0], X->filename, D[1]);
            out.write(")");
            break;
        }
        case AstNodeType::Macro: {
            auto *X = cast<Macro>(ast);
            out.write("Macro(", X->macro, ")");
            break;
        }
        case AstNodeType::Using: {
            auto *X = cast<Using>(ast);
            out.write("Using(", X->namespc, ")");
            break;
        }
        case AstNodeType::TypeSpec: {
            auto *X = cast<TypeSpec>(ast);
            out.write("Type");
            if (X->reference) {
                out.write("<&>");
            }
            out.write("(");
            Print(X->id);
            if (X->HasQualifiers()) {
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
            auto *X = cast<EnumDecl>(ast);
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
            auto *X = cast<TypedefDecl>(ast);
            out.write("TypedefDecl(\"", X->decl->name, "\" = ");
            Print(X->decl->typespec);
            out.write(")");
            break;
        }
        case AstNodeType::StructDecl: {
            auto *X = cast<StructDecl>(ast);
            out.write("StructDecl('", X->name, "', {");
            out.endln();
            out.indent();
            for (DeclStmt *decl : X->decls) {
                out.beginln();
                Print(decl);
                out.endln();
            }
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::FuncDecl: {
            auto *X = cast<FuncDecl>(ast);
            out.write("FuncDecl(");
            Print(X->id);
            out.write(", ");
            Print(X->return_typespec);
            out.write(", Params = {");
            for (int i = 0; i < X->params.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                out.write('"', X->params[i]->name, "\": ");
                Print(X->params[i]->typespec);
            }
            if (X->block) {
                out.write("}, {");
                out.endln();
                out.indent();
                out.beginln();
                Print(X->block);
                out.endln();
                out.dedent();
                out.beginln();
            }
            out.write("})");
            break;
        }
        case AstNodeType::Block: {
            auto *X = cast<Block>(ast);
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
                Print(stmt);
                out.endln();
            }
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::Identifier: {
            auto *X = cast<Identifier>(ast);
            out.write("id:");
            if (!X->prefix.empty()) {
                out.write("[");
                for (int i = 0; i < X->prefix.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    Print(X->prefix[i]);
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
                    Print(X->subtypes[i]);
                }
                out.write(">");
            }
            break;
        }
        case AstNodeType::Literal: {
            auto *X = cast<Literal>(ast);
            if (X->paren) {
                out.write("(");
            }
            switch (X->kind) {
                case Literal::Bool:
                    out.write("Bool<", X->val.as_bool ? "true" : "false", ">");
                    break;
                case Literal::Char:
                    out.write("Char<", Literal::Escape(X->val.as_char, '\''), ">");
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
                    out.write("String<", Literal::Escape(*(X->val.as_string.s), '"'), ">");
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
            auto *X = cast<BinaryExpr>(ast);
            if (X->paren) {
                out.write("(");
            }
            out.write(X->op, "(");
            Print(X->left);
            out.write(", ");
            Print(X->right);
            out.write(")");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::VarDecl: {
            auto *X = cast<VarDecl>(ast);
            if (X->kind == Decl::Pointer) {
                out.write("*");
            }
            out.write('"', X->name, '"');
            break;
        }
        case AstNodeType::ArrayDecl: {
            auto *X = cast<ArrayDecl>(ast);
            out.write('"', X->name, "\"(");
            if (X->sizes.size() == 1) {
                out.write("Size = ");
                Print(X->sizes[0]);
            } else {
                out.write("Sizes = {");
                for (int i = 0; i < X->sizes.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    Print(X->sizes[i]);
                }
                out.write("}");
            }
            out.write(")");
            break;
        }
        case AstNodeType::ExprList: {
            auto *X = cast<ExprList>(ast);
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
        case AstNodeType::ObjDecl: {
            auto *X = cast<ObjDecl>(ast);
            out.write('"', X->name, "\"(");
            if (!X->args.empty()) {
                out.write("Args = {");
                for (int i = 0; i < X->args.size(); i++) {
                    if (i > 0) {
                        out.write(", ");
                    }
                    Print(X->args[i]);
                }
                out.write("}");
            }
            out.write(")");
            break;
        }
        case AstNodeType::DeclStmt: {
            auto *X = cast<DeclStmt>(ast);
            out.write("DeclStmt(");
            Print(X->typespec);
            out.write(", Vars = {");
            for (int i = 0; i < X->items.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                Print(X->items[i].decl);
                if (X->items[i].init) {
                    out.write(" = ");
                    Print(X->items[i].init);
                }
            }
            out.write("})");
            break;
        }
        case AstNodeType::ExprStmt: {
            auto *X = cast<ExprStmt>(ast);
            out.write("ExprStmt");
            if (X->is_return) {
                out.write("<return>");
            }
            out.write("(");
            if (X->expr) {
                Print(X->expr);
            }
            out.write(")");
            break;
        }
        case AstNodeType::IfStmt: {
            auto *X = cast<IfStmt>(ast);
            out.write("IfStmt(");
            Print(X->cond);
            out.write(", ");
            Print(X->then);
            if (X->els) {
                out.write(", ");
                Print(X->els);
            }
            out.write(")");
            break;
        }
        case AstNodeType::ForStmt: {
            auto *X = cast<ForStmt>(ast);
            out.write("ForStmt(");
            if (X->init) {
                Print(X->init);
            } else {
                out.write("_");
            }
            out.write(", ");
            if (X->cond) {
                Print(X->cond);
            } else {
                out.write("_");
            }
            out.write(", ");
            if (X->post) {
                Print(X->post);
            } else {
                out.write("_");
            }
            out.write(", {");
            out.endln();
            out.indent();
            out.beginln();
            Print(X->substmt);
            out.endln();
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::WhileStmt: {
            auto *X = cast<WhileStmt>(ast);
            out.write("WhileStmt(");
            Print(X->cond);
            out.write(", {");
            out.endln();
            out.indent();
            out.beginln();
            Print(X->substmt);
            out.endln();
            out.dedent();
            out.beginln();
            out.write("})");
            break;
        }
        case AstNodeType::JumpStmt: {
            auto         *X = cast<JumpStmt>(ast);
            static string keyword[] = {"break", "continue", "goto"};
            out.write("JumpStmt<", keyword[X->kind], ">(");
            if (X->kind == JumpStmt::Goto) {
                out.write('"', X->label, '"');
            }
            out.write(")");
            break;
        }
        case AstNodeType::CallExpr: {
            auto *X = cast<CallExpr>(ast);
            out.write("CallExpr(");
            Print(X->func);
            out.write(", Args = {");
            for (int i = 0; i < X->args.size(); i++) {
                if (i > 0) {
                    out.write(", ");
                }
                Print(X->args[i]);
            }
            out.write("})");
            break;
        }
        case AstNodeType::IndexExpr: {
            auto *X = cast<IndexExpr>(ast);
            out.write("IndexExpr(");
            Print(X->base);
            out.write(", ");
            Print(X->index);
            out.write(")");
            break;
        }
        case AstNodeType::FieldExpr: {
            auto *X = cast<FieldExpr>(ast);
            out.write("FieldExpr");
            if (X->pointer) {
                out.write("<pointer>");
            }
            out.write("(");
            Print(X->base);
            out.write(", '", X->field, "')");
            break;
        }
        case AstNodeType::CondExpr: {
            auto *X = cast<CondExpr>(ast);
            if (X->paren) {
                out.write("(");
            }
            out.write("CondExpr(");
            Print(X->cond);
            out.write(", ");
            Print(X->then);
            out.write(", ");
            Print(X->els);
            out.write(")");
            if (X->paren) {
                out.write(")");
            }
            break;
        }
        case AstNodeType::SignExpr: {
            auto *X = cast<SignExpr>(ast);
            out.write("SignExpr<", (X->kind == SignExpr::Positive ? '+' : '-'), ">(");
            Print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::NegExpr: {
            auto *X = cast<NegExpr>(ast);
            out.write("NegExpr(");
            Print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::IncrExpr: {
            auto *X = cast<IncrExpr>(ast);
            out.write("IncrExpr<", X->kind == IncrExpr::Positive ? "++" : "--");
            out.write(", ", (X->preincr ? "pre" : "post"), ">(");
            Print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::AddrExpr: {
            auto *X = cast<AddrExpr>(ast);
            out.write("AddrExpr(");
            Print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::DerefExpr: {
            auto *X = cast<DerefExpr>(ast);
            out.write("DerefExpr(");
            Print(X->expr);
            out.write(")");
            break;
        }
        case AstNodeType::StmtError: {
            auto *X = cast<StmtError>(ast);
            out.write("ErrorStmt(\"", X->code, "\")");
            break;
        }
        case AstNodeType::ExprError: {
            auto *X = cast<ExprError>(ast);
            out.write("ErrorExpr(\"", X->code, "\")");
            break;
        }
        default:
            out.write("<error>");
            break;
    }
}

void ast_print(AstNode *ast, ostream& out) {
    AstPrinter(out).Print(ast);
}
