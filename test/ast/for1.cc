void xxx(string s) {
   for (a = 1; a < 100; a = a + 1) {
      cout * a;
      i = x;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("xxx", Type(void), Params = {"s": Type(string)}, {
      Block({
         IterStmt<for>(ExprStmt(=(id:'a', lit:'1')), <(id:'a', lit:'100'), =(id:'a', +(id:'a', lit:'1')), {
            Block({
               ExprStmt(*(id:'cout', id:'a'))
               ExprStmt(=(id:'i', id:'x'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
