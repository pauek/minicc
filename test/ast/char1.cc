string f() {
   '\n' + d + '\t';
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(+(+(Char<\n>, id:'d'), Char<\t>))
      })
   })
}
[[err]]--------------------------------------------------
