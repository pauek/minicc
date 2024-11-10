#ifndef WALKER_HH
#define WALKER_HH
#include <assert.h>
#include <iostream>
#include <vector>
#include "ast.hh"
#include "cast.h"

template <typename Delegate>
struct Walker {
    Delegate& D;

    Walker(Delegate& D_) : D(D_) {}

    void walk(AstNode *ast);
};

template <typename Delegate>
void Walker<Delegate>::walk(AstNode *ast) {
    switch (ast->Type()) {
        case AstNodeType::Program: {
            Program *X = cast<Program>(ast);
            D.walk(X);
            for (AstNode *n : X->nodes) {
                walk(n);
            }
            break;
        }
        case AstNodeType::Include: {
            Include *X = cast<Include>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::Using: {
            Using *X = cast<Using>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::Macro: {
            Macro *X = cast<Macro>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::Literal: {
            Literal *X = cast<Literal>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::JumpStmt: {
            JumpStmt *X = cast<JumpStmt>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::StmtError: {
            StmtError *X = cast<StmtError>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::ExprError: {
            ExprError *X = cast<ExprError>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::VarDecl: {
            VarDecl *X = cast<VarDecl>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::ArrayDecl: {
            ArrayDecl *X = cast<ArrayDecl>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::ObjDecl: {
            ObjDecl *X = cast<ObjDecl>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::EnumDecl: {
            EnumDecl *X = cast<EnumDecl>(ast);
            D.walk(X);
            break;
        }
        case AstNodeType::Identifier: {
            Identifier *X = cast<Identifier>(ast);
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
            TypeSpec *X = cast<TypeSpec>(ast);
            D.walk(X);
            if (X->id) {
                walk(X->id);
            }
            break;
        }
        case AstNodeType::TypedefDecl: {
            TypedefDecl *X = cast<TypedefDecl>(ast);
            D.walk(X);
            if (X->decl) {
                walk(X->decl);
            }
            break;
        }
        case AstNodeType::StructDecl: {
            StructDecl *X = cast<StructDecl>(ast);
            D.walk(X);
            for (auto decl : X->decls) {
                walk(decl);
            }
            break;
        }
        case AstNodeType::FuncDecl: {
            FuncDecl *X = cast<FuncDecl>(ast);
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
            Block *X = cast<Block>(ast);
            D.walk(X);
            for (Stmt *s : X->stmts) {
                if (s) {
                    walk(s);
                }
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(ast);
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
            DeclStmt *X = cast<DeclStmt>(ast);
            D.walk(X);
            if (X->typespec) {
                walk(X->typespec);
            }
            for (DeclStmt::Item item : X->items) {
                walk(item.decl);
                if (item.init) {
                    walk(item.init);
                }
            }
            break;
        }
        case AstNodeType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::IfStmt: {
            IfStmt *X = cast<IfStmt>(ast);
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
            WhileStmt *X = cast<WhileStmt>(ast);
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
            ForStmt *X = cast<ForStmt>(ast);
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
            CallExpr *X = cast<CallExpr>(ast);
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
            IndexExpr *X = cast<IndexExpr>(ast);
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
            FieldExpr *X = cast<FieldExpr>(ast);
            D.walk(X);
            if (X->base) {
                walk(X->base);
            }
            break;
        }
        case AstNodeType::CondExpr: {
            CondExpr *X = cast<CondExpr>(ast);
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
            ExprList *X = cast<ExprList>(ast);
            D.walk(X);
            for (Expr *e : X->exprs) {
                if (e) {
                    walk(e);
                }
            }
            break;
        }
        case AstNodeType::SignExpr: {
            SignExpr *X = cast<SignExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::NegExpr: {
            NegExpr *X = cast<NegExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::AddrExpr: {
            AddrExpr *X = cast<AddrExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstNodeType::DerefExpr: {
            DerefExpr *X = cast<DerefExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
    }
}

template <typename Delegate>
void walk(AstNode *ast, Delegate&& D) {
    Walker<Delegate>(D).walk(ast);
}

std::vector<Error *> collect_errors(AstNode *X);
#endif
