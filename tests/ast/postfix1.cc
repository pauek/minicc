int f() {
    (a+b)->c[7](1, 2, 3) = 7;
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'f', Type(id:'int'), Params = {}, {
        Block({
            ExprStmt(=(CallExpr(IndexExpr(FieldExpr<pointer>((+(id:'a', id:'b')), 'c'), Int<7>), Args = {Int<1>, Int<2>, Int<3>}), Int<7>))
        })
    })
}
