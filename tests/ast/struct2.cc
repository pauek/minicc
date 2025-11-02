struct X {
    vector<int> a, b;
    double x, y; // yay
};
[[out]]--------------------------------------------------
Program{
    StructDecl('X', {
        DeclStmt(Type(id:'vector'<Type(id:'int')>), Vars = {"a", "b"})
        DeclStmt(Type(id:'double'), Vars = {"x", "y"})
    })
}
