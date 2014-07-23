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
         IterStmt<while>(>=(id:'a', Int<20>), {
            Block({
               ExprStmt(=(id:'a', +(id:'a', Int<2>)))
               ExprStmt(=(id:'i', id:'a'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
