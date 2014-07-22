void f() {
   g();
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         Stmt(expr, CallExpr(id:'g', Args = {}))
      })
   })
}
[[err]]--------------------------------------------------
