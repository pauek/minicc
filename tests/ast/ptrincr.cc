int f() {
    *a++;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            ExprStmt(DerefExpr(IncrExpr<++, post>(id:'a')))
        })
    })
}
