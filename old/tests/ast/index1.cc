int f() {
   a[1] = 0;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(=(IndexExpr(id:'a', Int<1>), Int<0>))
      })
   })
}
