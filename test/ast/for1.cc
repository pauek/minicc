void xxx(string s) {
   for (a = 1; a < 100; a = a + 1) {
      cout << a;
      i = x;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'xxx', Type(id:'void'), Params = {"s": Type(id:'string')}, {
      Block({
         IterStmt<for>(ExprStmt(=(id:'a', Int<1>)), <(id:'a', Int<100>), =(id:'a', +(id:'a', Int<1>)), {
            Block({
               ExprStmt(<<(id:'cout', id:'a'))
               ExprStmt(=(id:'i', id:'x'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
