void f() {
     vector<int> v;
     for (int x : get_vector()) {
          cout << x << endl;
     }
}
[[out]]----------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            DeclStmt(Type(id:'vector'<Type(id:'int')>), Vars = {"v"})
            ForColonStmt(DeclStmt(Type(id:'int'), Vars = {"x"}) : CallExpr(id:'get_vector', Args = {}), {
                Block({
                    ExprStmt(<<(<<(id:'cout', id:'x'), id:'endl'))
                })
            })
        })
    })
}