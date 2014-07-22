void f() {
   g();
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {}))
      })
   })
}
[[err]]--------------------------------------------------
