#include <iostream>
using namespace std;

#include "ast.hh"
#include "instrumenter.hh"
#include "parser.hh"

constexpr const char *VAR_NAME = "__inst_iterations__";

ExprStmt *parse_expr_stmt(std::string code) {
    istringstream iss(code);
    Parser        parser(&iss);
    return parser.parse_exprstmt(nullptr);
}

void add_variable(Block *block, std::string name, TypeSpec *typespec) {
    auto stmt = new DeclStmt(typespec);
    stmt->add({new VarDecl(name), new Literal(Literal::Int, {.as_int = 0})});
    block->stmts.insert(block->stmts.begin(), stmt);
}

void add_var_incr(Block *block, std::string name) {
    auto incr = new IncrExpr(IncrExpr::Kind::Positive, true);
    incr->expr = new Identifier(name);
    auto stmt = new ExprStmt(incr);
    block->stmts.insert(block->stmts.begin(), stmt);
}

void increment_variable_in_loops(Block *block, std::string name) {
    for (auto node : block->stmts) {
        if (node->type() == AstNodeType::WhileStmt) {
            WhileStmt *stmt = cast<WhileStmt>(node);
            if (stmt->substmt->type() == AstNodeType::Block) {
                Block *block = cast<Block>(stmt->substmt);
                add_var_incr(block, name);
            }
        }
    }
}

void show_variable_at_end(Block *block, std::string name) {
    block->stmts.push_back(parse_expr_stmt("std::cerr << " + name + " << std::endl;"));
}

void Instrumenter::instrument(AstNode *node) {
    assert(node->type() == AstNodeType::Program);
    auto *program = cast<Program>(node);

    auto t_int = new TypeSpec(new Identifier("int"));

    for (auto node : program->nodes) {
        if (node->type() == AstNodeType::FuncDecl) {
            FuncDecl *fn = cast<FuncDecl>(node);
            if (fn->func_name() == "main") {
                add_variable(fn->block, VAR_NAME, t_int);
                increment_variable_in_loops(fn->block, VAR_NAME);
                show_variable_at_end(fn->block, VAR_NAME);
            }
        }
    }
}