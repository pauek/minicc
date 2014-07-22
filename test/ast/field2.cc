int f() {
   a->b = 3;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(FieldExpr<pointer>(id:'a', id:'b'), lit:'3'))
      })
   })
}
[[err]]--------------------------------------------------
