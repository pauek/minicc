void xxx(string s) {
   while (a >= 20) {
      a = a + 2;
      i = a;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("xxx", Type(void), Params = {"s": Type(string)}, {
      Block({
         IterStmt<while>(>=(id:'a', lit:'20'), {
            Block({
               Stmt(expr, =(id:'a', +(id:'a', lit:'2')))
               Stmt(expr, =(id:'i', id:'a'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
