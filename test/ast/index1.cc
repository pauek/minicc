int f() {
   a[1] = 0;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(=(IndexExpr(id:'a', lit:'1'), lit:'0'))
      })
   })
}
[[err]]--------------------------------------------------
