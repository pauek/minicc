void f() {
   -++a;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(SignExpr<->(IncrExpr<++, pre>(id:'a')))
      })
   })
}
