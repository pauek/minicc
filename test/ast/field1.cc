int f() {
   a.b = 3;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr(id:'a', id:'b'), Int<3>))
      })
   })
}
[[err]]--------------------------------------------------
