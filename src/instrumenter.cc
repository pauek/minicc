#include "instrumenter.hh"

void Instrumenter::instrument(AstNodeCore *node) {
    Ast *ast = node->ast;
    assert(node->type() == AstNodeType::Program);
    auto *program = cast<Program>(node);

    auto id = ast->create_node<Identifier>();
    id->name = "int";
    auto type = ast->create_node<TypeSpec>();
    type->id = id;
    auto vardecl = ast->create_node<VarDecl>();
    vardecl->typespec = type;
    vardecl->name = "a";

    auto stmt = ast->create_node<DeclStmt>();
    stmt->typespec = type;
    stmt->items.push_back({vardecl});

    program->nodes.insert(program->nodes.begin(), stmt);
}