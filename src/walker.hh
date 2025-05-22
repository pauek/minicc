#ifndef WALKER_HH
#define WALKER_HH
#include <assert.h>
#include <iostream>
#include <vector>
#include "ast.hh"

template <typename Delegate>
struct Walker {
    Delegate& D;

    Walker(Delegate& D_) : D(D_) {}

    void walk(AstNodeCore *ast);
};

template <typename Delegate>
void Walker<Delegate>::walk(AstNodeCore *node) {
    switch (node->type()) {
        case AstNodeType::Program: {
            auto *X = cast<Program>(node);
            D.walk(X);
            for (AstNodeCore *n : X->nodes) {
                walk(n);
            }
            break;
        }
        case AstNodeType::Include: {
            auto *X = cast<Include>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::Using: {
            auto *X = cast<Using>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::Macro: {
            auto *X = cast<Macro>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::Literal: {
            auto *X = cast<Literal>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::JumpStmt: {
            auto *X = cast<JumpStmt>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::StmtError: {
            auto *X = cast<StmtError>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::ExprError: {
            auto *X = cast<ExprError>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::VarDecl: {
            auto *X = cast<VarDecl>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::ArrayDecl: {
            auto *X = cast<ArrayDecl>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::ObjDecl: {
            auto *X = cast<ObjDecl>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::EnumDecl: {
            auto *X = cast<EnumDecl>(node);
            D.walk(X);
            break;
        }
        case AstNodeType::Identifier: {
            auto *X = cast<Identifier>(node);
            D.walk(X);
            for (Identifier *pre : X->prefix) {
                walk(pre);
            }
            for (TypeSpec *spec : X->subtypes) {
                walk(spec);
            }
            break;
        }
        case AstNodeType::TypeSpec: {
            auto *X = cast<TypeSpec>(node);
            D.walk(X);
            if (X->id) {
                walk(X->id);
            }
            break;
        }
        case AstNodeType::TypedefDecl: {
            auto *X = cast<TypedefDecl>(node);
            D.walk(X);
            if (X->decl) {
                walk(X->decl);
            }
            break;
        }
        case AstNodeType::StructDecl: {
            auto *X = cast<StructDecl>(node);
            D.walk(X);
            for (auto decl : X->decls) {
                walk(decl);
            }
            break;
        }
        case AstNodeType::FuncDecl: {
            auto *X = cast<FuncDecl>(node);
            D.walk(X);
            if (X->return_typespec) {
                walk(X->return_typespec);
            }
            if (X->block) {
                walk(X->block);
            }
            break;
        }
        case AstNodeType::Block: {
            auto *X = cast<Block>(node);
            D.walk(X);
            for (Stmt *s : X->stmts) {
                if (s) {
                    walk(s);
                }
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            auto *X = cast<BinaryExpr>(node);
            D.walk(X);
            if (X->left) {
                walk(X->left);
            }
            if (X->right) {
                walk(X->right);
            }
            break;
        }
        case AstNodeType::DeclStmt: {
            auto *X = cast<DeclStmt>(node);
            D.walk(X);
            if (X->typespec) {
                walk(X->typespec);
            }
            for (auto& item : X->items) {
                walk(item.decl);
                if (item.init) {
                    walk(item.init);
                }
            }
            break;
        }
        case AstNodeType::ExprStmt: {
            auto *X = cast<ExprStmt>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::IfStmt: {
            auto *X = cast<IfStmt>(node);
            D.walk(X);
            if (X->cond) {
                walk(X->cond);
            }
            if (X->then) {
                walk(X->then);
            }
            if (X->els) {
                walk(X->els);
            }
            break;
        }
        case AstNodeType::WhileStmt: {
            auto *X = cast<WhileStmt>(node);
            D.walk(X);
            if (X->cond) {
                walk(X->cond);
            }
            if (X->substmt) {
                walk(X->substmt);
            }
            break;
        }
        case AstNodeType::ForStmt: {
            auto *X = cast<ForStmt>(node);
            D.walk(X);
            if (X->init) {
                walk(X->init);
            }
            if (X->cond) {
                walk(X->cond);
            }
            if (X->post) {
                walk(X->post);
            }
            if (X->substmt) {
                walk(X->substmt);
            }
            break;
        }
        case AstNodeType::CallExpr: {
            auto *X = cast<CallExpr>(node);
            D.walk(X);
            if (X->func) {
                walk(X->func);
            }
            for (int i = 0; i < X->args.size(); i++) {
                walk(X->args[i]);
            }
            break;
        }
        case AstNodeType::IndexExpr: {
            auto *X = cast<IndexExpr>(node);
            D.walk(X);
            if (X->base) {
                walk(X->base);
            }
            if (X->index) {
                walk(X->index);
            }
            break;
        }
        case AstNodeType::FieldExpr: {
            auto *X = cast<FieldExpr>(node);
            D.walk(X);
            if (X->base) {
                walk(X->base);
            }
            break;
        }
        case AstNodeType::CondExpr: {
            auto *X = cast<CondExpr>(node);
            D.walk(X);
            if (X->cond) {
                walk(X->cond);
            }
            if (X->then) {
                walk(X->then);
            }
            if (X->els) {
                walk(X->els);
            }
            break;
        }
        case AstNodeType::ExprList: {
            auto *X = cast<ExprList>(node);
            D.walk(X);
            for (Expr *e : X->exprs) {
                if (e) {
                    walk(e);
                }
            }
            break;
        }
        case AstNodeType::SignExpr: {
            auto *X = cast<SignExpr>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::IncrExpr: {
            auto *X = cast<IncrExpr>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::NegExpr: {
            auto *X = cast<NegExpr>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::AddrExpr: {
            auto *X = cast<AddrExpr>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::DerefExpr: {
            auto *X = cast<DerefExpr>(node);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
    }
}

template <typename Delegate>
void walk(AstNodeCore *ast, Delegate&& D) {
    Walker<Delegate>(D).walk(ast);
}

std::vector<Error *> collect_errors(AstNodeCore *X);
#endif
