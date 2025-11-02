int f ( ) {
    int const a = 1;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            DeclStmt(Type(id:'int', {const}), Vars = {"a" = Int<1>})
        })
    })
}
