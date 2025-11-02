void f() {
    g();
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            ExprStmt(CallExpr(id:'g', Args = {}))
        })
    })
}
