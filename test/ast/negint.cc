void f() {
   a = -1;
   b = -0.5;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(=(id:'a', Int<-1>))
         ExprStmt(=(id:'b', Double<-0.5>))
      })
   })
}
[[err]]--------------------------------------------------
