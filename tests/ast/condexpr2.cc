void f() {
   a == b ? x = y : x = z;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(CondExpr(==(id:'a', id:'b'), =(id:'x', id:'y'), =(id:'x', id:'z')))
      })
   })
}
