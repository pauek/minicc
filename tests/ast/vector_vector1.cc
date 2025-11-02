int f ( ) {
    const vector<vector<int>> v;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            DeclStmt(Type(id:'vector'<Type(id:'vector'<Type(id:'int')>)>, {const}), Vars = {"v"})
        })
    })
}
