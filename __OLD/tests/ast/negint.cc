void f() {
   a = -1;
   b = -0.5;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(=(id:'a', SignExpr<->(Int<1>)))
         ExprStmt(=(id:'b', SignExpr<->(Double<0.5>)))
      })
   })
}
