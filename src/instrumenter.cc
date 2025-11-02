#include <iostream>
#include <set>
using namespace std;

#include "ast.hh"
#include "instrumenter.hh"
#include "parser.hh"
#include "pprint.hh"
#include "walker.hh"

constexpr const char *PREFIX = "__INSTRUMENTATION__";

constexpr const char *FUNC_EXEC = "function_calls";
constexpr const char *LOOP_ITER = "loop_iterations";

const vector<const char *> ALL_VARS = {FUNC_EXEC, LOOP_ITER};

struct Instrumenter {
    set<AstNode *> instrumented_nodes_;

    string varname(string name) { return PREFIX + name + "__"; }

    Stmt *parse_stmt_(string code, AstNode *parent = nullptr) {
        Stmt *stmt = parse_stmt(code, parent);
        instrumented_nodes_.insert(stmt);
        return stmt;
    }

    AstNode *parse_macro_(string code, AstNode *parent = nullptr) {
        AstNode *node = parse_macro(code, parent);
        instrumented_nodes_.insert(node);
        return node;
    }

    void add_loop_iterations_incr(Block *block) {
        auto& stmts = block->stmts;
        stmts.insert(stmts.begin(), parse_stmt_("++" + varname(LOOP_ITER) + ";", block));
    }

    void add_instrumentation_declarations(Program *program) {
        for (auto name : ALL_VARS) {
            program->comments.insert(
                program->comments.begin() + 1, new CommentSeq({Comment(Comment::Kind::EndLine)})
            );
            AstNode *new_node = parse_stmt_("size_t " + varname(name) + " = 0;", program);
            program->nodes.insert(program->nodes.begin(), new_node);
        }
        program->comments.insert(
            program->comments.begin() + 1, new CommentSeq({Comment(Comment::Kind::EndLine)})
        );
        program->nodes.insert(program->nodes.begin(), parse_macro_("#include <cstddef>", program));
    }

    void add_instrumentation_output(Block *block) {
        auto& stmts = block->stmts;
        for (auto name : ALL_VARS) {
            stmts.insert(
                stmts.end(),
                parse_stmt_(
                    "std::cout << \"" + string(name) + " \" << " + varname(name) + " << std::endl;",
                    block
                )
            );
        }
    }

    void walk(AstNode *node) {
        switch (node->type()) {
            case AstNodeType::Program: {
                auto *program = cast<Program>(node);
                add_instrumentation_declarations(program);
                break;
            }
            case AstNodeType::FuncDecl: {
                auto *fndecl = cast<FuncDecl>(node);
                if (fndecl->block != nullptr) {
                    if (fndecl->func_name() == "main") {
                        add_instrumentation_output(fndecl->block);
                    } else {
                        auto& stmts = fndecl->block->stmts;
                        stmts.insert(
                            stmts.begin(), parse_stmt_("++" + varname(FUNC_EXEC) + ";\n", node)
                        );
                    }
                }
                break;
            }
            case AstNodeType::WhileStmt: {
                auto *whileStmt = cast<WhileStmt>(node);
                if (whileStmt->substmt->type() != AstNodeType::Block) {
                    whileStmt->substmt = new Block({whileStmt->substmt}, whileStmt);
                }
                add_loop_iterations_incr(cast<Block>(whileStmt->substmt));
                break;
            }
            case AstNodeType::ForStmt: {
                auto *forStmt = cast<ForStmt>(node);
                if (forStmt->substmt->type() != AstNodeType::Block) {
                    forStmt->substmt = new Block({forStmt->substmt}, forStmt);
                }
                add_loop_iterations_incr(cast<Block>(forStmt->substmt));
                break;
            }
            case AstNodeType::ForColonStmt: {
                auto *forColonStmt = cast<ForColonStmt>(node);
                if (forColonStmt->substmt->type() != AstNodeType::Block) {
                    forColonStmt->substmt = new Block({forColonStmt->substmt}, forColonStmt);
                }
                add_loop_iterations_incr(cast<Block>(forColonStmt->substmt));
                break;
            }
            case AstNodeType::BinaryExpr: {
                auto *expr = cast<BinaryExpr>(node);
                if (expr->parent == nullptr || expr->parent->type() != AstNodeType::ExprStmt) {
                    break;
                }
                // Do not instrument already instrumented nodes ;)
                auto it = instrumented_nodes_.find(expr->parent);
                if (it != instrumented_nodes_.end()) {
                    break;
                }
                auto left_most = expr->left;
                while (left_most->type() == AstNodeType::BinaryExpr) {
                    left_most = cast<BinaryExpr>(left_most)->left;
                }
                auto sleft = spprint(expr->left);
                if (sleft != "cout" && sleft != "std::cout") {
                    break;
                }
                if (expr->op != "<<") {
                    break;
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