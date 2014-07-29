void f() {
   -++a;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(SignExpr<->(IncrExpr<++, pre>(id:'a')))
      })
   })
}
[[err]]--------------------------------------------------
