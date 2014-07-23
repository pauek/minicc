void f() {
   g(a, b, c);
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {id:'a', id:'b', id:'c'}))
      })
   })
}
[[err]]--------------------------------------------------
