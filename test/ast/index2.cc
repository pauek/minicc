int f() {
   a[b + 1] = 0;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         Stmt(expr, =(IndexExpr(id:'a', +(id:'b', lit:'1')), lit:'0'))
      })
   })
}
[[err]]--------------------------------------------------
