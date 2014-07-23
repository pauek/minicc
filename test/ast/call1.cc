void f() {
   g();
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'g', Args = {}))
      })
   })
}
[[err]]--------------------------------------------------
