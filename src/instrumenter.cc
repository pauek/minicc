#include "instrumenter.hh"

void Instrumenter::instrument(AstNode *node) {
    assert(node->type() == AstNodeType::Program);
    auto *program = cast<Program>(node);

    auto id = new Identifier("int");
    auto type = new TypeSpec();
    type->id = id;
    auto vardecl = new VarDecl();
    vardecl->typespec = type;
    vardecl->name = "a";

    auto stmt = new DeclStmt();
    stmt->typespec = type;
    stmt->items.push_back({vardecl});

    program->nodes.insert(program->nodes.begin(), stmt);
}