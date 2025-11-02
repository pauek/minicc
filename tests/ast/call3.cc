void f() {
    g(a, b, c);
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            ExprStmt(CallExpr(id:'g', Args = {id:'a', id:'b', id:'c'}))
        })
    })
}
