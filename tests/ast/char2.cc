string f() {
   main('Z', ' ', 'r');
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'main', Args = {Char<Z>, Char< >, Char<r>}))
      })
   })
}
