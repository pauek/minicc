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
         IterStmt<for>(Stmt(expr, =(id:'a', lit:'1')), <(id:'a', lit:'100'), =(id:'a', +(id:'a', lit:'1')), {
            Block({
               Stmt(expr, *(id:'cout', id:'a'))
               Stmt(expr, =(id:'i', id:'x'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
