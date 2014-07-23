string f() {
   "abc\"" + d + "\"efg";
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(string), Params = {}, {
      Block({
         ExprStmt(+(+(lit:'abc\"', id:'d'), lit:'\"efg'))
      })
   })
}
[[err]]--------------------------------------------------
