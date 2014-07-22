void f() {
   -a++;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(SignExpr<->(IncrExpr<++, post>(id:'a')))
      })
   })
}
[[err]]--------------------------------------------------
