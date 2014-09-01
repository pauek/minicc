string f() {
   "abc\"" + d + "\"efg";
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(+(+(String<abc\">, id:'d'), String<\"efg>))
      })
   })
}
[[err]]--------------------------------------------------
