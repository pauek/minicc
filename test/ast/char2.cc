string f() {
   main('bla', ' ', 'r');
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(CallExpr(id:'main', Args = {Char<bla>, Char< >, Char<r>}))
      })
   })
}
[[err]]--------------------------------------------------
