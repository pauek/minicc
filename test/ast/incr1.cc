void f() {
   a++;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(IncrExpr<++, post>(id:'a'))
      })
   })
}
[[err]]--------------------------------------------------
