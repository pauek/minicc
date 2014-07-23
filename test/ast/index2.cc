int f() {
   a[b + 1] = 0;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(=(IndexExpr(id:'a', +(id:'b', Int<1>)), Int<0>))
      })
   })
}
[[err]]--------------------------------------------------
