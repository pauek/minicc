void f() {
   g(a + b);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         Stmt(expr, CallExpr(id:'g', Args = {+(id:'a', id:'b')}))
      })
   })
}
[[err]]--------------------------------------------------
