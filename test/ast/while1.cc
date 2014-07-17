void xxx(string s) {
   while (a = true) {
      a = a + 2;
      i = a;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("xxx", Type(void), Params = {"s": Type(string)}, {
      Block({
         Stmt(while, =(id:'a', lit:'true'), {
            Block({
               Stmt(expr, =(id:'a', +(id:'a', lit:'2')))
               Stmt(expr, =(id:'i', id:'a'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
