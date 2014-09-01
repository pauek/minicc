int f() {
   if (a == b) {
      cout + x;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'int'), Params = {}, {
      Block({
         IfStmt(==(id:'a', id:'b'), Block({
            ExprStmt(+(id:'cout', id:'x'))
         }))
      })
   })
}
[[err]]--------------------------------------------------
