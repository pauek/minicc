int f() {
    a.b = 3;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            ExprStmt(=(FieldExpr(id:'a', 'b'), Int<3>))
        })
    })
}
