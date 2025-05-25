#include <iostream>
using namespace std;

#include "ast.hh"
#include "instrumenter.hh"
#include "parser.hh"
#include "walker.hh"

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
            } else {
                auto block = new Block();
                add_var_incr(block, name);
                block->stmts.push_back(stmt->substmt);
                stmt->substmt = block;
            }
        }
    }
}

void show_variable_at_end(Block *block, std::string name) {
    block->stmts.push_back(parse_expr_stmt("std::cerr << " + name + " << std::endl;"));
}

Stmt *incr_instrumentation_variable(string name) {
    return new ExprStmt(new IncrExpr(new Identifier("__instr__" + name + "__")));
}

Stmt *decl_instrumentation_variable(string name) {
    auto *stmt = new DeclStmt(
        new TypeSpec(new Identifier("size_t")),
        {
            {new VarDecl("__instr__" + name + "__"), new Literal(Literal::Int, {.as_int = 0})}
    }
    );
    return stmt;
}

void add_instrumentation_declarations(Program *program) {
    program->comments.insert(
        program->comments.begin() + 1,
        new CommentSeq({Comment(Comment::Kind::EndLine), Comment(Comment::Kind::EndLine)})
    );
    auto *decl = decl_instrumentation_variable("func_exec");
    program->nodes.insert(program->nodes.begin(), decl);
    decl->parent = program;
}

Stmt *show_instrumentation_variable(std::string name) {
    return new ExprStmt(new BinaryExpr(
        BinaryExpr::Kind::Shift,
        "<<",
        new Identifier("cerr"),
        new BinaryExpr(
            BinaryExpr::Kind::Shift,
            "<<",
            new Identifier("__instr__" + name + "__"),
            new Identifier("endl")
        )
    ));
}

struct Instrumenter {
    void walk(AstNode *node) {
        switch (node->type()) {
            case AstNodeType::Program: {
                auto *program = cast<Program>(node);
                add_instrumentation_declarations(program);
                break;
            }
            case AstNodeType::FuncDecl: {
                auto *fndecl = cast<FuncDecl>(node);
                auto& stmts = fndecl->block->stmts;
                auto *var = incr_instrumentation_variable("func_exec");
                stmts.insert(stmts.begin(), var);
                var->parent = node;

                if (fndecl->func_name() == "main") {
                    auto *stmt = show_instrumentation_variable("func_exec");
                    stmts.push_back(stmt);
                    stmt->parent = fndecl->block;
                }
                break;
            }
            default:
                break;
        }
    }
};

void instrument(AstNode *node) {
    walk(node, Instrumenter());
}