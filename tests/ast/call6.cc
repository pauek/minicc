void f() {
    a + b(1);
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            ExprStmt(+(id:'a', CallExpr(id:'b', Args = {Int<1>})))
        })
    })
}
