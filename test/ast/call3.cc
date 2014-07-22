void f() {
   g(a, b, c);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         Stmt(expr, CallExpr(id:'g', Args = {id:'a', id:'b', id:'c'}))
      })
   })
}
[[err]]--------------------------------------------------
