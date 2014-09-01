string f() {
   "\a\b\fabc\"" + d + "\"efg" + h;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(+(+(+(String<\a\b\fabc\">, id:'d'), String<\"efg>), id:'h'))
      })
   })
}
[[err]]--------------------------------------------------
