int f() {
   (a+b)->c[7](1, 2, 3) = 7;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(CallExpr(IndexExpr(FieldExpr<pointer>((+(id:'a', id:'b')), id:'c'), Int<7>), Args = {Int<1>, Int<2>, Int<3>}), Int<7>))
      })
   })
}
[[err]]--------------------------------------------------
