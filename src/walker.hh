#ifndef WALKER_HH
#define WALKER_HH

#include <assert.h>
#include <iostream>
#include <vector>
#include "ast.hh"
#include "cast.h"

template<typename Delegate>
struct Walker {
   Delegate& D;
   Walker(Delegate& D_) : D(D_) {}
   void Walk(Ast *ast);
};

template<typename Delegate>
void Walker<Delegate>::Walk(Ast *ast) {
   switch (ast->type()) {
      case AstType::Program: {
         Program *X = cast<Program>(ast);
         D.Walk(X);
         for (Ast* n : X->nodes) {
            Walk(n);
         }
         break;
      }
      case AstType::Include: {
         Include *X = cast<Include>(ast);
         D.Walk(X);
         break;
      }
      case AstType::Using: {
         Using *X = cast<Using>(ast);
         D.Walk(X);
         break;
      }
      case AstType::Macro: {
         Macro *X = cast<Macro>(ast);
         D.Walk(X);
         break;
      }
      case AstType::Literal: {
         Literal *X = cast<Literal>(ast);
         D.Walk(X);
         break;
      }
      case AstType::JumpStmt: {
         JumpStmt *X = cast<JumpStmt>(ast);
         D.Walk(X);
         break;
      }
      case AstType::StmtError: {
         StmtError *X = cast<StmtError>(ast);
         D.Walk(X);
         break;
      }
      case AstType::ExprError: {
         ExprError *X = cast<ExprError>(ast);
         D.Walk(X);
         break;
      }
      case AstType::VarDecl: {
         VarDecl *X = cast<VarDecl>(ast);
         D.Walk(X);
         break;
      }
      case AstType::ArrayDecl: {
         ArrayDecl *X = cast<ArrayDecl>(ast);
         D.Walk(X);
         break;
      }
      case AstType::ObjDecl: {
         ObjDecl *X = cast<ObjDecl>(ast);
         D.Walk(X);
         break;
      }
      case AstType::EnumDecl: {
         EnumDecl *X = cast<EnumDecl>(ast);
         D.Walk(X);
         break;
      }
      case AstType::Identifier: {
         Identifier *X = cast<Identifier>(ast);
         D.Walk(X);
         for (Identifier *pre : X->prefix) {
            Walk(pre);
         }
         for (TypeSpec *spec : X->subtypes) {
            Walk(spec);
         }
         break;
      }
      case AstType::TypeSpec: {
         TypeSpec *X = cast<TypeSpec>(ast);
         D.Walk(X);
         if (X->id) {
            Walk(X->id);
         }
         break;
      }
      case AstType::TypedefDecl: {
         TypedefDecl *X = cast<TypedefDecl>(ast);
         D.Walk(X);
         if (X->decl) {
            Walk(X->decl);
         }
         break;
      }
      case AstType::StructDecl: {
         StructDecl *X = cast<StructDecl>(ast);
         D.Walk(X);
         for (auto decl : X->decls) {
            Walk(decl);
         }
         break;
      }
      case AstType::FuncDecl: {
         FuncDecl *X = cast<FuncDecl>(ast);
         D.Walk(X);
         if (X->return_typespec) {
            Walk(X->return_typespec);
         }
         if (X->block) {
            Walk(X->block);
         }
         break;
      }
      case AstType::Block: {
         Block *X = cast<Block>(ast);
         D.Walk(X);
         for (Stmt *s : X->stmts) {
            if (s) {
               Walk(s);
            }
         }
         break;
      }
      case AstType::BinaryExpr: {
         BinaryExpr *X = cast<BinaryExpr>(ast);
         D.Walk(X);
         if (X->left) {
            Walk(X->left);
         }
         if (X->right) {
            Walk(X->right);
         }
         break;
      }
      case AstType::DeclStmt: {
         DeclStmt *X = cast<DeclStmt>(ast);
         D.Walk(X);
         if (X->typespec) {
            Walk(X->typespec);
         }
         for (DeclStmt::Item item : X->items) {
            Walk(item.decl);
            if (item.init) {
               Walk(item.init);
            }
         }
         break;
      }
      case AstType::ExprStmt: {
         ExprStmt *X = cast<ExprStmt>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         } 
         break;
      }
      case AstType::IfStmt: {
         IfStmt *X = cast<IfStmt>(ast);
         D.Walk(X);
         if (X->cond) {
            Walk(X->cond);
         }
         if (X->then) {
            Walk(X->then);
         }
         if (X->els) {
            Walk(X->els);
         }
         break;
      }
      case AstType::WhileStmt: {
         WhileStmt *X = cast<WhileStmt>(ast);
         D.Walk(X);
         if (X->cond) {
            Walk(X->cond);
         }
         if (X->substmt) {
            Walk(X->substmt);
         }
         break;
      }
      case AstType::ForStmt: {
         ForStmt *X = cast<ForStmt>(ast);
         D.Walk(X);
         if (X->init) {
            Walk(X->init);
         }
         if (X->cond) {
            Walk(X->cond);
         }
         if (X->post) {
            Walk(X->post);
         }
         if (X->substmt) {
            Walk(X->substmt);
         }
         break;
      }
      case AstType::CallExpr: {
         CallExpr *X = cast<CallExpr>(ast);
         D.Walk(X);
         if (X->func) {
            Walk(X->func);
         }
         for (int i = 0; i < X->args.size(); i++) {
            Walk(X->args[i]);
         }
         break;
      }
      case AstType::IndexExpr: {
         IndexExpr *X = cast<IndexExpr>(ast);
         D.Walk(X);
         if (X->base) {
            Walk(X->base);
         }
         if (X->index) {
            Walk(X->index);
         }
         break;
      }
      case AstType::FieldExpr: {
         FieldExpr *X = cast<FieldExpr>(ast);
         D.Walk(X);
         if (X->base) {
            Walk(X->base);
         }
         break;
      }
      case AstType::CondExpr: {
         CondExpr *X = cast<CondExpr>(ast);
         D.Walk(X);
         if (X->cond) {
            Walk(X->cond);
         }
         if (X->then) {
            Walk(X->then);
         }
         if (X->els) {
            Walk(X->els);
         }
         break;
      }
      case AstType::ExprList: {
         ExprList *X = cast<ExprList>(ast);
         D.Walk(X);
         for (Expr *e : X->exprs) {
            if (e) {
               Walk(e);
            }
         }
         break;
      }
      case AstType::SignExpr: {
         SignExpr *X = cast<SignExpr>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         }
         break;
      }
      case AstType::IncrExpr: {
         IncrExpr *X = cast<IncrExpr>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         }
         break;
      }
      case AstType::NegExpr: {
         NegExpr *X = cast<NegExpr>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         }
         break;
      }
      case AstType::AddrExpr: {
         AddrExpr *X = cast<AddrExpr>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         }
         break;
      }
      case AstType::DerefExpr: {
         DerefExpr *X = cast<DerefExpr>(ast);
         D.Walk(X);
         if (X->expr) {
            Walk(X->expr);
         }
         break;
      }
   }
}

template<typename Delegate>
void Walk(Ast *ast, Delegate& D) {
   Walker<Delegate>(D).Walk(ast);
}

void collect_errors(Ast *X, std::vector<Error*>& v);

#endif
