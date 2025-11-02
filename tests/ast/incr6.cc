void f() {
    a--;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'void'), Params = {}, {
        Block({
            ExprStmt(IncrExpr<--, post>(id:'a'))
        })
    })
}
