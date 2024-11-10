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

    void walk(Ast *ast);
};

template <typename Delegate>
void Walker<Delegate>::walk(Ast *ast) {
    switch (ast->Type()) {
        case AstType::Program: {
            Program *X = cast<Program>(ast);
            D.walk(X);
            for (Ast *n : X->nodes) {
                walk(n);
            }
            break;
        }
        case AstType::Include: {
            Include *X = cast<Include>(ast);
            D.walk(X);
            break;
        }
        case AstType::Using: {
            Using *X = cast<Using>(ast);
            D.walk(X);
            break;
        }
        case AstType::Macro: {
            Macro *X = cast<Macro>(ast);
            D.walk(X);
            break;
        }
        case AstType::Literal: {
            Literal *X = cast<Literal>(ast);
            D.walk(X);
            break;
        }
        case AstType::JumpStmt: {
            JumpStmt *X = cast<JumpStmt>(ast);
            D.walk(X);
            break;
        }
        case AstType::StmtError: {
            StmtError *X = cast<StmtError>(ast);
            D.walk(X);
            break;
        }
        case AstType::ExprError: {
            ExprError *X = cast<ExprError>(ast);
            D.walk(X);
            break;
        }
        case AstType::VarDecl: {
            VarDecl *X = cast<VarDecl>(ast);
            D.walk(X);
            break;
        }
        case AstType::ArrayDecl: {
            ArrayDecl *X = cast<ArrayDecl>(ast);
            D.walk(X);
            break;
        }
        case AstType::ObjDecl: {
            ObjDecl *X = cast<ObjDecl>(ast);
            D.walk(X);
            break;
        }
        case AstType::EnumDecl: {
            EnumDecl *X = cast<EnumDecl>(ast);
            D.walk(X);
            break;
        }
        case AstType::Identifier: {
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
        case AstType::TypeSpec: {
            TypeSpec *X = cast<TypeSpec>(ast);
            D.walk(X);
            if (X->id) {
                walk(X->id);
            }
            break;
        }
        case AstType::TypedefDecl: {
            TypedefDecl *X = cast<TypedefDecl>(ast);
            D.walk(X);
            if (X->decl) {
                walk(X->decl);
            }
            break;
        }
        case AstType::StructDecl: {
            StructDecl *X = cast<StructDecl>(ast);
            D.walk(X);
            for (auto decl : X->decls) {
                walk(decl);
            }
            break;
        }
        case AstType::FuncDecl: {
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
        case AstType::Block: {
            Block *X = cast<Block>(ast);
            D.walk(X);
            for (Stmt *s : X->stmts) {
                if (s) {
                    walk(s);
                }
            }
            break;
        }
        case AstType::BinaryExpr: {
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
        case AstType::DeclStmt: {
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
        case AstType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstType::IfStmt: {
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
        case AstType::WhileStmt: {
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
        case AstType::ForStmt: {
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
        case AstType::CallExpr: {
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
        case AstType::IndexExpr: {
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
        case AstType::FieldExpr: {
            FieldExpr *X = cast<FieldExpr>(ast);
            D.walk(X);
            if (X->base) {
                walk(X->base);
            }
            break;
        }
        case AstType::CondExpr: {
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
        case AstType::ExprList: {
            ExprList *X = cast<ExprList>(ast);
            D.walk(X);
            for (Expr *e : X->exprs) {
                if (e) {
                    walk(e);
                }
            }
            break;
        }
        case AstType::SignExpr: {
            SignExpr *X = cast<SignExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstType::NegExpr: {
            NegExpr *X = cast<NegExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstType::AddrExpr: {
            AddrExpr *X = cast<AddrExpr>(ast);
            D.walk(X);
            if (X->expr) {
                walk(X->expr);
            }
            break;
        }
        case AstType::DerefExpr: {
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
void walk(Ast *ast, Delegate& D) {
    Walker<Delegate>(D).walk(ast);
}

std::vector<Error *> collect_errors(Ast *X);
#endif
