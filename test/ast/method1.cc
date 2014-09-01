int f() {
   map.find("x");
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         ExprStmt(CallExpr(FieldExpr(id:'map', id:'find'), Args = {String<x>}))
      })
   })
}
[[err]]--------------------------------------------------
