int f() {
   *a++;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(DerefExpr(IncrExpr<++, post>(id:'a')))
      })
   })
}
[[err]]--------------------------------------------------
