void f() {
   a == b ? 1 : 2;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CondExpr(==(id:'a', id:'b'), Int<1>, Int<2>))
      })
   })
}
[[err]]--------------------------------------------------
