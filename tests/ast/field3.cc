int f() {
    (a+b)->c = 7;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            ExprStmt(=(FieldExpr<pointer>((+(id:'a', id:'b')), 'c'), Int<7>))
        })
    })
}
