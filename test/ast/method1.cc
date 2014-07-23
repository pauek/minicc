int f() {
   map.find("x");
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         ExprStmt(CallExpr(FieldExpr(id:'map', id:'find'), Args = {String<x>}))
      })
   })
}
[[err]]--------------------------------------------------
