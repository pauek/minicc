string f() {
   .01;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(id:'string'), Params = {}, {
      Block({
         ExprStmt(Double<0.01>)
      })
   })
}
[[err]]--------------------------------------------------
