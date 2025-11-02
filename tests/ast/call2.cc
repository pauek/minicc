void f() {
    g(a + b);
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            ExprStmt(CallExpr(id:'g', Args = {+(id:'a', id:'b')}))
        })
    })
}
