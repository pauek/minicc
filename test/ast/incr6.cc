void f() {
   a--;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(IncrExpr<--, post>(id:'a'))
      })
   })
}
[[err]]--------------------------------------------------
