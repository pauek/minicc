void f() {
   g(a + 1, b + 2);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {+(id:'a', lit:'1'), +(id:'b', lit:'2')}))
      })
   })
}
[[err]]--------------------------------------------------
